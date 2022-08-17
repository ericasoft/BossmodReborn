﻿using Dalamud.Game.ClientState.JobGauge.Types;
using System;
using System.Linq;

namespace BossMod.WHM
{
    // TODO: this is shit, like all healer modules...
    class Actions : HealerActions
    {
        public const int AutoActionST = AutoActionFirstCustom + 0;
        public const int AutoActionAOE = AutoActionFirstCustom + 1;
        public const int AutoActionSTHeal = AutoActionFirstCustom + 2;
        public const int AutoActionAOEHeal = AutoActionFirstCustom + 3;

        private WHMConfig _config;
        private Rotation.State _state;
        private Rotation.Strategy _strategy;
        private (Actor? Target, float HPRatio) _bestSTHeal;
        private bool _allowDelayingNextGCD;

        public Actions(Autorotation autorot, Actor player)
            : base(autorot, player, Definitions.QuestsPerLevel, Definitions.SupportedActions)
        {
            _config = Service.Config.Get<WHMConfig>();
            _state = new(autorot.Cooldowns);
            _strategy = new();

            // upgrades
            SupportedSpell(AID.Stone1).TransformAction = SupportedSpell(AID.Stone2).TransformAction = SupportedSpell(AID.Stone3).TransformAction = SupportedSpell(AID.Stone4).TransformAction
                = SupportedSpell(AID.Glare1).TransformAction = SupportedSpell(AID.Glare3).TransformAction = () => ActionID.MakeSpell(_state.BestGlare);
            SupportedSpell(AID.Aero1).TransformAction = SupportedSpell(AID.Aero2).TransformAction = SupportedSpell(AID.Dia).TransformAction = () => ActionID.MakeSpell(_state.BestDia);
            SupportedSpell(AID.Holy1).TransformAction = SupportedSpell(AID.Holy3).TransformAction = () => ActionID.MakeSpell(_state.BestHoly);

            SupportedSpell(AID.DivineBenison).Condition = target => target != null && target.FindStatus(SID.DivineBenison, Player.InstanceID) == null; // check whether potential divine benison target doesn't already have it applied

            _config.Modified += OnConfigModified;
            OnConfigModified(null, EventArgs.Empty);
        }

        public override void Dispose()
        {
            _config.Modified -= OnConfigModified;
            base.Dispose();
        }

        public override Targeting SelectBetterTarget(Actor initial)
        {
            // TODO: select target for holy...

            // look for target to multidot, if initial target already has dot
            if (_state.Unlocked(MinLevel.Aero1) && !WithoutDOT(initial))
            {
                var multidotTarget = Autorot.PotentialTargetsInRangeFromPlayer(25).FirstOrDefault(t => t != initial && WithoutDOT(t));
                if (multidotTarget != null)
                    return new(multidotTarget, 10);
            }

            return new(initial, 10);
        }

        protected override void UpdateInternalState(int autoAction)
        {
            base.UpdateInternalState(autoAction);
            UpdatePlayerState();
            FillCommonStrategy(_strategy, CommonDefinitions.IDPotionMnd);
            _strategy.NumAssizeMedica1Targets = _state.Unlocked(MinLevel.Medica1) ? CountAOEHealTargets(15, Player.Position) : 0;
            _strategy.NumRaptureMedica2Targets = _state.Unlocked(MinLevel.Medica2) ? CountAOEHealTargets(20, Player.Position) : 0;
            _strategy.NumCure3Targets = _state.Unlocked(MinLevel.Cure3) ? SmartCure3Target().Item2 : 0;
            _strategy.NumHolyTargets = _state.Unlocked(MinLevel.Holy1) ? Autorot.PotentialTargetsInRangeFromPlayer(8).Count() : 0;
            _strategy.EnableAssize = AllowAssize(); // note: should be plannable...
            _strategy.AllowReplacingHealWithMisery = _config.NeverOvercapBloodLilies && Autorot.PrimaryTarget?.Type == ActorType.Enemy;
            _strategy.Heal = _strategy.AOE = false;
            _strategy.Moving = autoAction is AutoActionAIIdleMove or AutoActionAIFightMove;
            _bestSTHeal = FindBestSTHealTarget();
            if (autoAction >= AutoActionFirstCustom)
            {
                _strategy.Heal = autoAction is AutoActionSTHeal or AutoActionAOEHeal;
                _strategy.AOE = autoAction is AutoActionAOE or AutoActionAOEHeal;
            }
        }

        protected override void QueueAIActions()
        {
        }

        protected override NextAction CalculateAutomaticGCD()
        {
            // TODO: rework, especially non-ai...
            switch (AutoAction)
            {
                case AutoActionST:
                    return Autorot.PrimaryTarget != null ? MakeResult(Rotation.GetNextBestSTDamageGCD(_state, _strategy), Autorot.PrimaryTarget) : new();
                case AutoActionAOE:
                    return Autorot.PrimaryTarget != null ? MakeResult(Rotation.GetNextBestAOEDamageGCD(_state, _strategy), Autorot.PrimaryTarget) : new();
                case AutoActionSTHeal:
                    return MakeResult(Rotation.GetNextBestSTHealGCD(_state, _strategy), (_config.MouseoverFriendly ? SmartTargetFriendly(Autorot.PrimaryTarget) : Autorot.PrimaryTarget) ?? Player);
                case AutoActionAOEHeal:
                    return MakeResult(Rotation.GetNextBestAOEHealGCD(_state, _strategy), SmartCure3Target().Item1 ?? Player);
                default:
                    // AI: aoe heals > st heals > esuna > damage
                    // i don't really think 'rotation/actions' split is particularly good fit for healers AI...
                    // TODO: raise support...
                    bool allowCasts = !_strategy.Moving || _state.SwiftcastLeft > _state.GCD;

                    // TODO: L52+ (afflatus rapture)
                    // 2. medica 2, if possible and useful, and buff is not already up; we consider it ok to overwrite last tick
                    if (allowCasts && _strategy.NumRaptureMedica2Targets > 2 && _state.CanCastMedica2 && _state.MedicaLeft <= _state.GCD + 2.5f)
                        return MakeResult(AID.Medica2, Player);
                    // 3. cure 3, if possible and useful
                    if (allowCasts && _strategy.NumCure3Targets > 0 && _state.CanCastCure3)
                        return MakeResult(AID.Cure3, SmartCure3Target().Item1 ?? Player);
                    // 4. medica 1, if possible and useful
                    if (allowCasts && _strategy.NumAssizeMedica1Targets > 0 && _state.CanCastMedica1)
                        return MakeResult(AID.Medica1, Player);

                    // now check ST heals (TODO: afflatus solace)
                    if (allowCasts && _bestSTHeal.Target != null)
                        return MakeResult(_state.CanCastCure2 ? AID.Cure2 : AID.Cure1, _bestSTHeal.Target);

                    // now check esuna
                    if (allowCasts)
                    {
                        var esunaTarget = FindEsunaTarget();
                        if (esunaTarget != null)
                            return MakeResult(AID.Esuna, esunaTarget);
                    }

                    // regen, if allowed
                    var regenTarget = _state.Unlocked(MinLevel.Regen) ? FindProtectTarget() : null;
                    if (regenTarget != null && StatusDetails(regenTarget, SID.Regen, Player.InstanceID).Left < _state.GCD)
                        return MakeResult(AID.Regen, regenTarget);

                    // finally perform damage rotation
                    if (allowCasts && _strategy.NumHolyTargets >= 3)
                        return MakeResult(_state.BestHoly, Player);

                    if (Autorot.PrimaryTarget != null)
                        return MakeResult(Rotation.GetNextBestSTDamageGCD(_state, _strategy), Autorot.PrimaryTarget);

                    return new(); // chill
            }
        }

        protected override NextAction CalculateAutomaticOGCD(float deadline)
        {
            if (deadline < float.MaxValue && _allowDelayingNextGCD)
                deadline += 0.4f + _state.AnimationLockDelay;

            NextAction res = new();
            if (_state.CanWeave(deadline - _state.OGCDSlotLength)) // first ogcd slot
                res = GetNextBestOGCD(deadline - _state.OGCDSlotLength);
            if (!res.Action && _state.CanWeave(deadline)) // second/only ogcd slot
                res = GetNextBestOGCD(deadline - _state.OGCDSlotLength);
            return res;
        }

        private NextAction GetNextBestOGCD(float deadline)
        {
            // TODO: L52+

            // benediction at extremely low hp (TODO: unless planned, tweak threshold)
            if (_bestSTHeal.Target != null && _bestSTHeal.HPRatio <= 0 && _state.Unlocked(MinLevel.Benediction) && _state.CanWeave(CDGroup.Benediction, 0.6f, deadline))
                return MakeResult(AID.Benediction, _bestSTHeal.Target);

            // swiftcast, if can't cast any gcd
            if (deadline >= 10000 && _strategy.Moving && _state.Unlocked(MinLevel.Swiftcast) && _state.CanWeave(CDGroup.Swiftcast, 0.6f, deadline))
                return MakeResult(AID.Swiftcast, Player);

            // pom (TODO: consider delaying until raidbuffs?)
            if (_state.Unlocked(MinLevel.PresenceOfMind) && _state.CanWeave(CDGroup.PresenceOfMind, 0.6f, deadline))
                return MakeResult(AID.PresenceOfMind, Player);

            // lucid dreaming, if we won't waste mana (TODO: revise mp limit)
            if (_state.CurMP <= 7000 && _state.Unlocked(MinLevel.LucidDreaming) && _state.CanWeave(CDGroup.LucidDreaming, 0.6f, deadline))
                return MakeResult(AID.LucidDreaming, Player);

            return new();
        }

        protected override void OnActionExecuted(ActionID action, Actor? target)
        {
            Log($"Executed {action} @ {target} [{_state}]");
        }

        protected override void OnActionSucceeded(ActorCastEvent ev)
        {
            Log($"Succeeded {ev.Action} @ {ev.MainTargetID:X} [{_state}]");
            // note: our GCD heals have cast time 2 => 1.6 under POM (minus haste), +0.1 anim lock => after them we have 0.3 or even slightly smaller window
            // we want to be able to cast at least one oGCD after them, even if it means slightly delaying next GCD
            _allowDelayingNextGCD = ev.Action.Type == ActionType.Spell && (AID)ev.Action.ID is AID.Medica1 or AID.Medica2 or AID.Cure2 or AID.Cure3;
        }

        private void UpdatePlayerState()
        {
            FillCommonPlayerState(_state);

            var gauge = Service.JobGauges.Get<WHMGauge>();
            _state.NormalLilies = gauge.Lily;
            _state.BloodLilies = gauge.BloodLily;
            _state.NextLilyIn = 30 - gauge.LilyTimer * 0.001f;

            _state.SwiftcastLeft = StatusDetails(Player, SID.Swiftcast, Player.InstanceID).Left;
            _state.ThinAirLeft = StatusDetails(Player, SID.ThinAir, Player.InstanceID).Left;
            _state.FreecureLeft = StatusDetails(Player, SID.Freecure, Player.InstanceID).Left;
            _state.MedicaLeft = StatusDetails(Player, SID.Medica2, Player.InstanceID).Left;

            _state.TargetDiaLeft = StatusDetails(Autorot.PrimaryTarget, _state.ExpectedDia, Player.InstanceID).Left;
        }

        private bool WithoutDOT(Actor a) => Rotation.RefreshDOT(_state, StatusDetails(a, _state.ExpectedDia, Player.InstanceID).Left);

        private void OnConfigModified(object? sender, EventArgs args)
        {
            // placeholders
            //SupportedSpell(AID.Stone1).PlaceholderForAuto = SupportedSpell(AID.Stone2).PlaceholderForAuto = SupportedSpell(AID.Stone3).PlaceholderForAuto = SupportedSpell(AID.Stone4).PlaceholderForAuto
            //    = SupportedSpell(AID.Glare1).PlaceholderForAuto = SupportedSpell(AID.Glare3).PlaceholderForAuto = _config.FullRotation ? AutoActionST : AutoActionNone;
            //SupportedSpell(AID.Holy1).PlaceholderForAuto = SupportedSpell(AID.Holy3).PlaceholderForAuto = _config.FullRotation ? AutoActionAOE : AutoActionNone;
            //SupportedSpell(AID.Cure1).PlaceholderForAuto = _config.FullRotation ? AutoActionSTHeal : AutoActionNone;
            //SupportedSpell(AID.Medica1).PlaceholderForAuto = _config.FullRotation ? AutoActionAOEHeal : AutoActionNone;

            // raise replacement
            SupportedSpell(AID.Raise).TransformAction = _config.SwiftFreeRaise ? () => ActionID.MakeSpell(SmartRaiseAction()) : null;

            // smart targets
            SupportedSpell(AID.Cure1).TransformTarget = SupportedSpell(AID.Cure2).TransformTarget = SupportedSpell(AID.Regen).TransformTarget
                = SupportedSpell(AID.AfflatusSolace).TransformTarget = SupportedSpell(AID.Raise).TransformTarget = SupportedSpell(AID.Esuna).TransformTarget
                = SupportedSpell(AID.Rescue).TransformTarget = SupportedSpell(AID.DivineBenison).TransformTarget = SupportedSpell(AID.Tetragrammaton).TransformTarget
                = SupportedSpell(AID.Benediction).TransformTarget = SupportedSpell(AID.Aquaveil).TransformTarget
                = _config.MouseoverFriendly ? SmartTargetFriendly : null;
            SupportedSpell(AID.Cure3).TransformTarget = _config.SmartCure3Target ? _ => SmartCure3Target().Item1 : _config.MouseoverFriendly ? SmartTargetFriendly : null;
        }

        private AID SmartRaiseAction()
        {
            // 1. swiftcast, if ready and not up yet
            if (_state.Unlocked(MinLevel.Swiftcast) && _state.SwiftcastLeft <= 0 && _state.CD(CDGroup.Swiftcast) <= 0)
                return AID.Swiftcast;

            // 2. thin air, if ready and not up yet
            if (_state.Unlocked(MinLevel.ThinAir) && _state.ThinAirLeft <= 0 && _state.CD(CDGroup.ThinAir) <= 60)
                return AID.ThinAir;

            return AID.Raise;
        }

        // select best target for cure3, such that most people are hit
        private (Actor?, int) SmartCure3Target()
        {
            var rsq = 30 * 30;
            return Autorot.WorldState.Party.WithoutSlot().Select(o => (o, (o.Position - Player.Position).LengthSq() <= rsq ? CountAOEHealTargets(10, o.Position) : -1)).MaxBy(oc => oc.Item2);
        }

        // check whether any targetable enemies are in assize range
        private bool AllowAssize()
        {
            return Autorot.PotentialTargetsInRangeFromPlayer(15).Any();
        }
    }
}
