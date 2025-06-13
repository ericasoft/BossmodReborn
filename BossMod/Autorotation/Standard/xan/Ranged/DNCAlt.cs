using BossMod.Autorotation.akechi;
using BossMod.DNC;
using FFXIVClientStructs.FFXIV.Client.Game.Gauge;
using static BossMod.AIHints;

namespace BossMod.Autorotation.xan;

public sealed class DNCAlt(RotationModuleManager manager, Actor player) : Attackxan<AID, TraitID>(manager, player)
{
    public enum Track { Partner = SharedTrack.Count, UsePot }
    public enum PartnerStrategy { Automatic, Manual }
    public enum PotionStrategy { None, Grade3 }

    public static RotationModuleDefinition Definition()
    {
        var def = new RotationModuleDefinition("xan-erica DNC", "Dancer", "Optimized rotation (xan-erica)|Ranged", "xan, erica", RotationModuleQuality.Good, BitMask.Build(Class.DNC), 100, 80);

        def.DefineShared().AddAssociatedActions(AID.TechnicalStep, AID.Devilment);

        def.Define(Track.Partner).As<PartnerStrategy>("Partner")
            .AddOption(PartnerStrategy.Automatic, "Auto", "Choose dance partner automatically (based on job aDPS)")
            .AddOption(PartnerStrategy.Manual, "Manual", "Do not choose dance partner automatically");

        def.Define(Track.UsePot).As<PotionStrategy>("Potion")
            .AddOption(PotionStrategy.None, "None", "Do not use any potions")
            .AddOption(PotionStrategy.Grade3, "Grade 3", "Use Grade 3 gemdraughts");

        return def;
    }

    public byte Feathers;
    public bool IsDancing;
    public bool IsForceST;
    public byte CompletedSteps;
    public uint NextStep;
    public byte Esprit;

    public float StandardStepLeft; // 15s max
    public float StandardFinishLeft; // 60s max
    public float TechStepLeft; // 15s max
    public float TechFinishLeft; // 20s max
    public float FlourishingFinishLeft; // 30s max, granted by tech step
    public float ImprovisationLeft; // 15s max
    public float ImprovisedFinishLeft; // 30s max
    public float DevilmentLeft; // 20s max
    public float SymmetryLeft; // 30s max
    public float FlowLeft; // 30s max
    public float FlourishingStarfallLeft; // 20s max
    public float ThreefoldLeft; // 30s max
    public float FourfoldLeft; // 30s max
    public float LastDanceLeft; // 30s max
    public float FinishingMoveLeft; // 30s max
    public float DanceOfTheDawnLeft; // 30s max
    public float BuffsLeft;

    private Enemy? BestFan4Target;
    private Enemy? BestRangedAOETarget;
    private Enemy? BestStarfallTarget;

    public int NumAOETargets;
    public int NumDanceTargets;

    public int NumFan4Targets;
    public int NumRangedAOETargets;
    public int NumStarfallTargets;

    private const float FinishDanceWindow = 0.5f;

    protected override float GetCastTime(AID aid) => 0;

    private bool HaveTarget(Enemy? primaryTarget) => NumAOETargets > 1 || primaryTarget != null;

    private static float GetApplicationDelay(AID aid) => aid switch
    {
        AID.StandardFinish or AID.SingleStandardFinish or AID.DoubleStandardFinish => 0.54f,
        _ => 0
    };

    public override void Exec(StrategyValues strategy, Enemy? primaryTarget)
    {
        SelectPrimaryTarget(strategy, ref primaryTarget, range: 25);

        var gauge = World.Client.GetGauge<DancerGauge>();
        var curStep = (uint)gauge.CurrentStep;

        Feathers = gauge.Feathers;
        IsDancing = gauge.DanceSteps[0] != 0;
        CompletedSteps = gauge.StepIndex;
        NextStep = curStep > 0 ? curStep + 15998 : curStep;
        Esprit = gauge.Esprit;

        IsForceST = strategy.Option(SharedTrack.AOE).As<AOEStrategy>() is AOEStrategy.ForceST;

        StandardStepLeft = StatusLeft(SID.StandardStep);
        StandardFinishLeft = StatusLeft(SID.StandardFinish);
        TechStepLeft = StatusLeft(SID.TechnicalStep);
        TechFinishLeft = StatusLeft(SID.TechnicalFinish);
        FlourishingFinishLeft = StatusLeft(SID.FlourishingFinish);
        ImprovisationLeft = StatusLeft(SID.Improvisation);
        ImprovisedFinishLeft = StatusLeft(SID.ImprovisedFinish);
        DevilmentLeft = StatusLeft(SID.Devilment);
        SymmetryLeft = Math.Max(StatusLeft(SID.SilkenSymmetry), StatusLeft(SID.FlourishingSymmetry));
        FlowLeft = Math.Max(StatusLeft(SID.SilkenFlow), StatusLeft(SID.FlourishingFlow));
        FlourishingStarfallLeft = StatusLeft(SID.FlourishingStarfall);
        ThreefoldLeft = StatusLeft(SID.ThreefoldFanDance);
        FourfoldLeft = StatusLeft(SID.FourfoldFanDance);
        LastDanceLeft = StatusLeft(SID.LastDanceReady);
        FinishingMoveLeft = StatusLeft(SID.FinishingMoveReady);
        DanceOfTheDawnLeft = StatusLeft(SID.DanceOfTheDawnReady);

        (BestFan4Target, NumFan4Targets) = SelectTarget(strategy, primaryTarget, 15, IsFan4Target);
        (BestRangedAOETarget, NumRangedAOETargets) = SelectTarget(strategy, primaryTarget, 25, IsSplashTarget);
        (BestStarfallTarget, NumStarfallTargets) = SelectTarget(strategy, primaryTarget, 25, Is25yRectTarget);

        NumDanceTargets = NumNearbyTargets(strategy, 15);
        NumAOETargets = NumMeleeAOETargets(strategy);

        BuffsLeft = Math.Max(Math.Max(StatusLeft(SID.Medicated), TechFinishLeft), DevilmentLeft);

        if (ImprovisationLeft > 0)
        {
            // Assume we are improvising while there is no target - once there is one we should finish improvisation
            // Or if we don't have time to wait any longer, let's finish this improvisation
            if (NumRangedAOETargets > 0 || ImprovisationLeft < GCDLength)
            {
                PushGCD(AID.ImprovisedFinish, Player, 100);
            }

            // We are either finishing improvization or still improvizing, so let's not do any further action that could interrupt it
            return;
        }

        if (Unlocked(AID.ClosedPosition)
            && strategy.Option(Track.Partner).As<PartnerStrategy>() == PartnerStrategy.Automatic
            && StatusLeft(SID.ClosedPosition) == 0
            && ReadyIn(AID.ClosedPosition) == 0
            && !IsDancing
            && FindDancePartner() is Actor partner)
            PushGCD(AID.ClosedPosition, partner);

        OGCD(strategy, primaryTarget);

        var approach = IsDancing || ReadyIn(AID.StandardStep) <= GCD || strategy.BuffsOk() && ReadyIn(AID.TechnicalStep) <= GCD;

        GoalZoneCombined(strategy, approach ? 15 : 25, Hints.GoalAOECircle(IsDancing ? 15 : 5), AID.StandardFinish, 2);

        if (IsDancing)
        {
            List<byte> steps = new List<byte>(4);
            steps.AddRange(gauge.DanceSteps);

            if (steps.Distinct().Count() == 4 && strategy.Option(Track.UsePot).As<PotionStrategy>() == PotionStrategy.Grade3)
            {
                Hints.ActionsToExecute.Push(ActionDefinitions.IDPotionDex, Player, 8000);
            }

            if (NextStep != 0)
                PushGCD((AID)NextStep, Player);

            if (ShouldFinishDance(StandardStepLeft, 1))
                PushGCD(AID.DoubleStandardFinish, Player);

            if (ShouldFinishDance(TechStepLeft, 0))
            {
                PushGCD(AID.QuadrupleTechnicalFinish, Player, 10000);

                if (!OnCooldown(AID.Devilment) || ReadyIn(AID.Devilment) < GCDLength)
                {
                    PushOGCD(AID.Devilment, Player, 9000, 0.0f);
                    PushOGCD(AID.Devilment, Player, 8000, ReadyIn(AID.Devilment) + 0.001f);
                }
            }

            return;
        }

        if (CountdownRemaining > 0)
        {
            if (CountdownRemaining is > 3.5f and < 15.5f && !IsDancing)
                PushGCD(AID.StandardStep, Player);

            return;
        }

        // Helps reset after cutscenes/deaths
        if (StandardFinishLeft <= 0 && !OnCooldown(AID.StandardStep) && NumDanceTargets > 0)
        {
            PushGCD(AID.StandardStep, Player, 100);
            return;
        }

        if (ShouldTechStep(strategy) && (!OnCooldown(AID.TechnicalStep) || ReadyIn(AID.TechnicalStep) <= GCDLength))
        {
            PushGCD(AID.TechnicalStep, Player, 100);
            return;
        }

        if (strategy.BuffsOk() && TechFinishLeft > 15 && DevilmentLeft == 0 && (!OnCooldown(AID.Devilment) || ReadyIn(AID.Devilment) < GCD))
        {
            ClearGCD();
            PushOGCD(AID.Devilment, Player, 100);
            return;
        }

        var shouldStdStep = ShouldStdStep(strategy);

        var canStarfall = FlourishingStarfallLeft > GCD && NumStarfallTargets > 0;
        var canFlow = CanFlow(primaryTarget, out var flowCombo);
        var canSymmetry = CanSymmetry(primaryTarget, out var symmetryCombo);
        var combo2 = NumAOETargets > 1 ? AID.Bladeshower : AID.Fountain;
        var haveCombo2 = Unlocked(combo2) && ComboLastMove == (NumAOETargets > 1 ? AID.Windmill : AID.Cascade);

        if (!IsForceST && DanceOfTheDawnLeft > GCD && DanceOfTheDawnLeft <= GCDLength && Esprit >= 50)
            PushGCD(AID.DanceOfTheDawn, BestRangedAOETarget, 4);

        if (!IsForceST && canStarfall && FlourishingStarfallLeft <= GCDLength)
            PushGCD(AID.StarfallDance, BestStarfallTarget, 3);

        // the targets for these two will be auto fixed if they are AOE actions
        if (!IsForceST && ReadyIn(AID.FinishingMove) < GCDLength && FinishingMoveLeft >= GCDLength && NumDanceTargets > 0)
            PushGCD(AID.FinishingMove, Player, 5);

        if (!IsForceST && DanceOfTheDawnLeft > GCD && Esprit >= 50)
            PushGCD(AID.DanceOfTheDawn, BestRangedAOETarget);

        if (!IsForceST && DanceOfTheDawnLeft > GCD + GCDLength && FlourishingFinishLeft > GCD && Esprit < 25)
            PushGCD(AID.Tillana, BestRangedAOETarget);

        // If standard step (not finishing move) is available for one of the next two GCD, lower the threshold for esprit to make sure we don't prioritize saber dance over standard step
        if (ReadyIn(AID.StandardStep) > GCDLength && ReadyIn(AID.StandardStep) <= 2 * GCDLength && ShouldSaberDance(strategy, 60) && FinishingMoveLeft < 2 * GCDLength)
            PushGCD(AID.SaberDance, BestRangedAOETarget);

        // If standard step or finishing move is available for the second next GCD, prioritize last dance for next GCD to make sure it's not overridden
        if (!IsForceST && LastDanceLeft > 0 && (ReadyIn(AID.StandardStep) > GCD && ReadyIn(AID.StandardStep) <= GCD + 1.1f * GCDLength || ReadyIn(AID.FinishingMove) > GCD && ReadyIn(AID.FinishingMove) <= GCD + 1.1f * GCDLength))
            PushGCD(AID.LastDance, BestRangedAOETarget, 3);

        if (ShouldSaberDance(strategy, 80))
            PushGCD(AID.SaberDance, BestRangedAOETarget);

        // With well aligned dance partner buffs, you might accidentally reach 100 very quickly from 80, so we make the margin a bit bigger during raid buff
        if (BuffsLeft > GCD && ShouldSaberDance(strategy, 75))
            PushGCD(AID.SaberDance, BestRangedAOETarget);

        if (!IsForceST && canStarfall)
            PushGCD(AID.StarfallDance, BestStarfallTarget);

        //If we do not have time to wait until next GCD, use Tillana since it has higher potency than Saber Dance
        if (!IsForceST && 0 < FlourishingFinishLeft && FlourishingFinishLeft < GCDLength && OnCooldown(AID.Devilment) && NumDanceTargets > 0)
            PushGCD(AID.Tillana, Player);

        //If we have technical finish for starting+finishing dance (2 GCDs) + 2 steps, do standard step ASAP
        if (!IsForceST && BuffsLeft > 2 * GCDLength + 2 && shouldStdStep)
        {
            PushGCD(AID.StandardStep, Player, 3);
        }

        // With well aligned dance partner buffs, you might accidentally reach 100 very quickly from 80, so we make the margin a bit bigger during raid buff
        if (BuffsLeft > GCD && ShouldSaberDance(strategy, 70))
            PushGCD(AID.SaberDance, BestRangedAOETarget);

        //If we have time to wait until next GCD, try to make sure we empty our esprit gauge first to not overcap
        if (!IsForceST && FlourishingFinishLeft > GCDLength && NumDanceTargets > 0 && Esprit < 50)
            PushGCD(AID.Tillana, Player);

        // Proc is about to expire
        if (!IsForceST && LastDanceLeft > GCD && LastDanceLeft < GCDLength + GCD)
            PushGCD(AID.LastDance, BestRangedAOETarget);

        // If standard step or finishing move is available for the third next GCD, prioritize last dance for next GCD to make sure it's not overridden
        if (!IsForceST && LastDanceLeft > 0 && (ReadyIn(AID.StandardStep) > GCD && ReadyIn(AID.StandardStep) <= GCD + 2 * GCDLength || ReadyIn(AID.FinishingMove) > GCD && ReadyIn(AID.FinishingMove) <= GCD + 2 * GCDLength))
            PushGCD(AID.LastDance, BestRangedAOETarget);

        if (BuffsLeft > GCD && ShouldSaberDance(strategy, 50))
            PushGCD(AID.SaberDance, BestRangedAOETarget);

        if (!IsForceST && LastDanceLeft > GCD && BuffsLeft > GCD && OnCooldown(AID.Devilment))
            PushGCD(AID.LastDance, BestRangedAOETarget);

        // Time left for technical + start technical + finish technical + last dance itself + 4 steps = 3 GCD + 4 + time left
        if (!IsForceST && LastDanceLeft > GCD && LastDanceLeft < 3 * GCDLength + ReadyIn(AID.TechnicalStep) + 4)
            PushGCD(AID.LastDance, BestRangedAOETarget);

        // If tech finish is over we can use standard step again.
        if (TechFinishLeft == 0 && shouldStdStep)
        {
            PushGCD(AID.StandardStep, Player, 3);
        }

        if (canFlow && FlowLeft <= GCDLength)
            PushGCD(flowCombo, primaryTarget);

        if (canSymmetry && SymmetryLeft <= GCDLength)
            PushGCD(symmetryCombo, primaryTarget);

        if (haveCombo2 && !CanFitGCD(World.Client.ComboState.Remaining, 2))
        {
            if (canFlow)
                PushGCD(flowCombo, primaryTarget);

            if (!CanFitGCD(World.Client.ComboState.Remaining, 1))
                PushGCD(combo2, primaryTarget);
        }

        if (canFlow)
            PushGCD(flowCombo, primaryTarget);

        if (canSymmetry)
            PushGCD(symmetryCombo, primaryTarget);

        if (haveCombo2)
            PushGCD(combo2, primaryTarget);

        if (NumAOETargets > 1 && Unlocked(AID.Windmill))
            PushGCD(AID.Windmill, Player);

        PushGCD(AID.Cascade, primaryTarget);

    }

    private void OGCD(StrategyValues strategy, Enemy? primaryTarget)
    {
        if (CountdownRemaining > 0)
        {
            if (CountdownRemaining is > 2 and < 10 && NextStep == 0 && PelotonLeft == 0)
                PushOGCD(AID.Peloton, Player);

            return;
        }

        if (IsDancing)
            return;

        if (strategy.BuffsOk() && TechFinishLeft > 10 && (!OnCooldown(AID.Devilment) || CanWeave(AID.Devilment)))
        {
            PushOGCD(AID.Devilment, Player, 100);
            return;
        }

        if (ReadyIn(AID.Devilment) > 55)
            PushOGCD(AID.Flourish, Player);

        if ((TechFinishLeft == 0 || OnCooldown(AID.Devilment)) && ThreefoldLeft > AnimLock && NumRangedAOETargets > 0 && CanWeave(AID.FanDanceIII))
            PushOGCD(AID.FanDanceIII, BestRangedAOETarget);

        var canF1 = ShouldSpendFeathers(strategy, primaryTarget);
        var f1ToUse = NumAOETargets > 1 && Unlocked(AID.FanDanceII) ? AID.FanDanceII : AID.FanDance;

        if (Feathers == 4 && canF1 && CanWeave(f1ToUse))
            PushOGCD(f1ToUse, primaryTarget);

        if (FourfoldLeft > AnimLock && NumFan4Targets > 0 && CanWeave(AID.FanDanceIV))
            PushOGCD(AID.FanDanceIV, BestFan4Target);

        if (canF1 && CanWeave(f1ToUse))
            PushOGCD(f1ToUse, primaryTarget);
    }

    private bool ShouldStdStep(StrategyValues strategy)
    {
        return (!OnCooldown(AID.StandardStep) || ReadyIn(AID.StandardStep) <= GCDLength) && NumDanceTargets > 0 && !IsForceST;
    }

    private bool ShouldTechStep(StrategyValues strategy)
    {
        if (!strategy.BuffsOk())
            return false;

        // Ensure the buffs align - wait if can't weave between tech finish and next GCD - i.e. extra 2 GCD (start, finish) + 4 steps
        if (Unlocked(AID.Devilment) && OnCooldown(AID.Devilment) && !CanWeave(AID.Devilment, 2, 4))
            return false;

        return NumDanceTargets > 0 && !IsForceST;
    }

    private bool CanFlow(Enemy? primaryTarget, out AID action)
    {
        var act = NumAOETargets > 1 ? AID.Bloodshower : AID.Fountainfall;
        if (Unlocked(act) && FlowLeft > GCD && HaveTarget(primaryTarget))
        {
            action = act;
            return true;
        }

        action = AID.None;
        return false;
    }

    private bool CanSymmetry(Enemy? primaryTarget, out AID action)
    {
        var act = NumAOETargets > 1 ? AID.RisingWindmill : AID.ReverseCascade;
        if (Unlocked(act) && SymmetryLeft > GCD && HaveTarget(primaryTarget))
        {
            action = act;
            return true;
        }

        action = AID.None;
        return false;
    }

    private bool ShouldFinishDance(float danceTimeLeft, int minimumTargets)
    {
        if (NextStep != 0)
            return false;
        if (danceTimeLeft is > 0 and < FinishDanceWindow)
            return true;

        return danceTimeLeft > GetApplicationDelay(AID.StandardFinish) && NumDanceTargets >= minimumTargets;
    }

    private bool ShouldSaberDance(StrategyValues strategy, int minimumEsprit)
    {
        if (Esprit < 50 || !Unlocked(AID.SaberDance))
            return false;

        return !IsForceST && Esprit >= minimumEsprit && NumRangedAOETargets > 0;
    }

    private bool ShouldSpendFeathers(StrategyValues strategy, Enemy? primaryTarget)
    {
        if (Feathers == 0)
            return false;

        // Always use feathers on cooldown if Tech Step isn't unlocked
        if (!Unlocked(AID.TechnicalStep))
            return true;

        // If allow risk of overcapping when very close to buff window to maximize chance of 4 feathers. Can probably be done better by combinning with below.
        if (strategy.BuffsOk() && ReadyIn(AID.TechnicalStep) < 4 * GCD)
            return false;

        // Keep 4 feathers until a flow or symmetry is available, to maximize feathers available during buff window
        if (Feathers == 4 && (CanFlow(primaryTarget, out _) || CanSymmetry(primaryTarget, out _)))
            return true;

        return BuffsLeft > AnimLock;
    }

    private bool IsFan4Target(Actor primary, Actor other) => Hints.TargetInAOECone(other, Player.Position, 15, Player.DirectionTo(primary), 60.Degrees());

    private Actor? FindDancePartner()
    {
        var partner = World.Party.WithoutSlot(excludeAlliance: true, excludeNPCs: true).Exclude(Player).Where(x => Player.DistanceToHitbox(x) <= 30).MaxBy(p => p.Class switch
        {
            Class.SAM => 100,
            Class.NIN or Class.VPR or Class.ROG => 99,
            Class.MNK or Class.PGL => 88,
            Class.RPR => 87,
            Class.DRG or Class.LNC => 86,
            Class.BLM or Class.PCT or Class.THM => 79,
            Class.SMN or Class.ACN => 78,
            Class.RDM => 77,
            Class.MCH => 69,
            Class.BRD or Class.ARC => 68,
            Class.DNC => 67,
            _ => 1
        });

        if (partner != null)
        {
            // target is in cutscene, we're probably in a raid or something - wait for it to finish
            if (World.Party.Members[World.Party.FindSlot(partner.InstanceID)].InCutscene)
                return null;

            return partner;
        }

        return World.Actors.FirstOrDefault(x => x.Type == ActorType.Chocobo && x.OwnerID == Player.InstanceID);
    }
}
