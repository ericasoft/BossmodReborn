﻿namespace BossMod.Autorotation.xan.AI;

public class TrackPartyHealth(WorldState World)
{
    public record struct PartyMemberState
    {
        public int Slot;
        public int PredictedHP;
        public int PredictedHPMissing;
        public float AttackerStrength;
        // predicted ratio including pending HP loss and current attacker strength
        public float PredictedHPRatio;
        // *actual* ratio including pending HP loss, used mainly just for essential dignity
        public float PendingHPRatio;
        // remaining time on cleansable status, to avoid casting it on a target that will lose the status by the time we finish
        public float EsunableStatusRemaining;
        // tank invulns go here, but also statuses like Excog that give burst heal below a certain HP threshold
        // no point in spam healing a tank in an area with high mob density (like Sirensong Sea pull after second boss) until their excog falls off
        public float NoHealStatusRemaining;
        // Doom (1769 and possibly other statuses) is only removed once a player reaches full HP, must be healed asap
        public float DoomRemaining;
        public Vector2 AveragePosition;
        public float MoveDelta;
        public DateTime LastCombat;
    }

    public record PartyHealthState
    {
        public int LowestHPSlot;
        public int Count;
        public float Avg;
        public float StdDev;
    }

    public const float AOEBreakpointHPVariance = 0.25f;

    public readonly PartyMemberState[] PartyMemberStates = new PartyMemberState[PartyState.MaxAllies];
    public PartyHealthState PartyHealth { get; private set; } = new();

    private bool _haveRealPartyMembers;
    private BitMask _trackedActors;

    public IEnumerable<(int, Actor)> TrackedMembers => World.Party.WithSlot().IncludedInMask(_trackedActors);

    // looking up this field in sheets is noticeably expensive somehow
    private static readonly Dictionary<uint, bool> _esunaCache = [];
    private static bool StatusIsRemovable(uint statusID)
    {
        if (_esunaCache.TryGetValue(statusID, out var value))
            return value;
        return _esunaCache[statusID] = Utils.StatusIsRemovable(statusID);
    }

    private static readonly uint[] NoHealStatuses = [
        82, // Hallowed Ground
        409, // Holmgang
        810, // Living Dead
        811, // Walking Dead
        1220, // Excogitation
        1836, // Superbolide
        2685, // Catharsis of Corundum
        (uint)WAR.SID.BloodwhettingDefenseLong
    ];
    private float StatusDuration(DateTime expireAt) => Math.Max((float)(expireAt - World.CurrentTime).TotalSeconds, 0.0f);

    private PartyHealthState CalculatePartyHealthState(Func<Actor, bool> filter)
    {
        int count = 0;
        float mean = 0;
        float m2 = 0;
        float min = float.MaxValue;
        int minSlot = -1;

        foreach (var p in PartyMemberStates)
        {
            var act = World.Party[p.Slot];
            if (act == null || !filter(act))
                continue;

            // player has tank invuln/excog/etc, skip them
            if (p.NoHealStatusRemaining > 1.5f && p.DoomRemaining == 0)
                continue;

            // no amount of healing can save player, skip them
            if (act.PendingHPDifferences.Any(p => -p.Value >= act.HPMP.MaxHP))
                continue;

            var pred = p.DoomRemaining > 0 ? 0 : p.PredictedHPRatio;
            if (pred < min)
            {
                min = pred;
                minSlot = p.Slot;
            }
            count++;
            var delta = pred - mean;
            mean += delta / count;
            var delta2 = pred - mean;
            m2 += delta * delta2;
        }

        var variance = m2 / count;
        return new PartyHealthState()
        {
            LowestHPSlot = minSlot,
            Avg = mean,
            StdDev = MathF.Sqrt(variance),
            Count = count
        };
    }

    private PartyHealthState CalcPartyHealthInArea(WPos center, float radius) => CalculatePartyHealthState(act => act.Position.InCircle(center, radius));

    public (Actor Target, PartyMemberState State)? BestSTHealTarget => PartyHealth.StdDev > AOEBreakpointHPVariance || PartyHealth.Count == 1 ? (World.Party[PartyHealth.LowestHPSlot]!, PartyMemberStates[PartyHealth.LowestHPSlot]) : null;

    public bool ShouldHealInArea(WPos center, float radius, float hpThreshold)
    {
        var st = CalcPartyHealthInArea(center, radius);
        // Service.Log($"party health in radius {radius}: {st}");
        return st.Count > 1 && st.StdDev <= AOEBreakpointHPVariance && st.Avg <= hpThreshold;
    }

    public void Update(AIHints Hints)
    {
        // copied from veyn's HealerActions in EW bossmod - i am a thief
        BitMask esunas = new();
        foreach (var caster in World.Party.WithoutSlot(excludeAlliance: true).Where(a => a.CastInfo?.IsSpell(BossMod.WHM.AID.Esuna) ?? false))
            esunas.Set(World.Party.FindSlot(caster.CastInfo!.TargetID));

        _haveRealPartyMembers = false;

        _trackedActors.Reset();

        for (var i = 0; i < PartyState.MaxAllies; ++i)
        {
            var shouldSkip = false;
            if (i >= PartyState.MaxPartySize)
            {
                // if we are running content with normal party, either duty support or human players, NPC allies should be ignored entirely
                if (_haveRealPartyMembers)
                    shouldSkip = true;

                // otherwise alliance should be skipped since healing actions generally can't target them
                if (i < PartyState.MaxAllianceSize)
                    shouldSkip = true;
            }

            var actor = World.Party[i];
            ref var state = ref PartyMemberStates[i];
            state.Slot = i;
            if (actor == null || actor.IsDead || actor.HPMP.MaxHP == 0 || actor.FateID > 0 || shouldSkip)
            {
                state.PredictedHP = state.PredictedHPMissing = 0;
                state.PredictedHPRatio = state.PendingHPRatio = 1;
            }
            else
            {
                _trackedActors[i] = true;
                state.PredictedHP = actor.PredictedHPRaw;
                state.PredictedHPMissing = (int)actor.HPMP.MaxHP - state.PredictedHP;
                state.PredictedHPRatio = state.PendingHPRatio = (float)state.PredictedHP / actor.HPMP.MaxHP;
                state.AttackerStrength = 0;
                state.EsunableStatusRemaining = 0;
                state.DoomRemaining = 0;
                state.NoHealStatusRemaining = 0;
                var canEsuna = actor.IsTargetable && !esunas[i];
                foreach (var s in actor.Statuses)
                {
                    if (canEsuna && StatusIsRemovable(s.ID))
                        state.EsunableStatusRemaining = Math.Max(StatusDuration(s.ExpireAt), state.EsunableStatusRemaining);

                    if (NoHealStatuses.Contains(s.ID))
                        state.NoHealStatusRemaining = StatusDuration(s.ExpireAt);

                    if (s.ID == 1769)
                        state.DoomRemaining = StatusDuration(s.ExpireAt);
                }

                if (actor.InCombat)
                    state.LastCombat = World.CurrentTime;

                var pos = actor.Position.ToVec2();
                if (state.AveragePosition == default)
                    state.AveragePosition = pos;
                else
                {
                    state.AveragePosition -= state.AveragePosition * World.Frame.Duration;
                    state.AveragePosition += pos * World.Frame.Duration;
                }
                state.MoveDelta = (state.AveragePosition - pos).Length();
            }
        }

        foreach (var enemy in Hints.PotentialTargets)
        {
            var targetSlot = World.Party.FindSlot(enemy.Actor.TargetID);
            if (_trackedActors[targetSlot])
            {
                ref var state = ref PartyMemberStates[targetSlot];
                state.AttackerStrength += enemy.AttackStrength;
                if (state.PredictedHPRatio < 0.99f)
                    state.PredictedHPRatio -= enemy.AttackStrength;
            }
        }

        foreach (var predicted in Hints.PredictedDamage)
            foreach (var bit in predicted.players.SetBits())
                PartyMemberStates[bit].PredictedHPRatio -= 0.30f;

        PartyHealth = CalculatePartyHealthState(_ => true);
    }
}
