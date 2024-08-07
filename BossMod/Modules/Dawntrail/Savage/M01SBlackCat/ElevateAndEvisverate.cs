namespace BossMod.Dawntrail.Savage.M01SBlackCat;

class ElevateAndEviscerateShockwave(BossModule module) : Components.GenericAOEs(module, default, "GTFO from shockwave!")
{
    private static readonly AOEShapeCross cross = new(60, 5);
    private readonly ElevateAndEviscerate _kb = module.FindComponent<ElevateAndEviscerate>()!;

    public override IEnumerable<AOEInstance> ActiveAOEs(int slot, Actor actor)
    {
        if (_kb.CurrentTarget != null && _kb.CurrentTarget != actor)
        {
            yield return new(cross, ArenaChanges.CellCenter(ArenaChanges.CellIndex(_kb.CurrentTarget.Position)), default, _kb.CurrentDeadline);
            if (_kb.CurrentKnockbackDistance == 0)
                yield return new(Mouser.Rect, ArenaChanges.CellCenter(ArenaChanges.CellIndex(_kb.CurrentTarget.Position)), default, _kb.CurrentDeadline);
            else if (_kb.CurrentKnockbackDistance == 10 && _kb.Sources(slot, _kb.CurrentTarget).Any())
                yield return new(Mouser.Rect, ArenaChanges.CellCenter(ArenaChanges.CellIndex(_kb.CalculateMovements(slot, _kb.CurrentTarget).First().to)), default, _kb.CurrentDeadline);
        }
    }

    public override void OnCastFinished(Actor caster, ActorCastInfo spell)
    {
        if ((AID)spell.Action.ID is AID.ElevateAndEviscerateKnockback or AID.ElevateAndEviscerateImpactHit)
            ++NumCasts;
    }
}

class ElevateAndEviscerate(BossModule module) : Components.Knockback(module, ignoreImmunes: true, stopAfterWall: true)
{
    private Actor? _nextTarget; // target selection icon appears before cast start
    public Actor? CurrentTarget; // for current mechanic
    public DateTime CurrentDeadline; // for current target - expected time when stun starts, which is deadline for positioning
    public int CurrentKnockbackDistance;

    public override PlayerPriority CalcPriority(int pcSlot, Actor pc, int playerSlot, Actor player, ref uint customColor)
        => player == CurrentTarget ? PlayerPriority.Danger : PlayerPriority.Irrelevant;

    public override IEnumerable<Source> Sources(int slot, Actor actor)
    {
        if (CurrentTarget != null && actor == CurrentTarget && CurrentKnockbackDistance > 0)
            yield return new(Arena.Center, CurrentKnockbackDistance, CurrentDeadline, Direction: actor.Rotation, Kind: Kind.DirForward);
    }

    public override void OnEventIcon(Actor actor, uint iconID)
    {
        if (iconID != (uint)IconID.ElevateAndEviscerate)
            return;
        if (_nextTarget != null)
            ReportError($"Next target icon before previous was consumed");
        _nextTarget = actor;
    }

    public override void AddHints(int slot, Actor actor, TextHints hints)
    {
        base.AddHints(slot, actor, hints);
        if (CurrentTarget == actor)
            hints.Add($"{(CurrentKnockbackDistance > 0 ? "Knockback" : "Hit")} in {Math.Max(0, (CurrentDeadline - Module.WorldState.CurrentTime).TotalSeconds):f1}s", false);
    }

    public override void OnCastStarted(Actor caster, ActorCastInfo spell)
    {
        if ((AID)spell.Action.ID is AID.ElevateAndEviscerateKnockback or AID.ElevateAndEviscerateHit)
        {
            if (_nextTarget == null)
            {
                ReportError("Cast started before target selection");
                return;
            }

            CurrentTarget = _nextTarget;
            CurrentDeadline = Module.CastFinishAt(spell, 1.8f);
            CurrentKnockbackDistance = (AID)spell.Action.ID == AID.ElevateAndEviscerateKnockback ? 10 : 0;
            _nextTarget = null;
        }
    }

    public override void OnEventCast(Actor caster, ActorCastEvent spell)
    {
        if ((AID)spell.Action.ID == AID.ElevateAndEviscerateShockwave)
        {
            ++NumCasts;
            CurrentTarget = null;
            CurrentDeadline = default;
            CurrentKnockbackDistance = 0;
        }
    }

    public override bool DestinationUnsafe(int slot, Actor actor, WPos pos) => (Module.FindComponent<ElevateAndEviscerateHint>()?.ActiveAOEs(slot, actor).Any(z => z.Shape.Check(pos, z.Origin, z.Rotation)) ?? false) || !Module.InBounds(pos);
}

class ElevateAndEviscerateHint(BossModule module) : Components.GenericAOEs(module)
{
    public override IEnumerable<AOEInstance> ActiveAOEs(int slot, Actor actor)
    {
        var comp = Module.FindComponent<ElevateAndEviscerate>()!.CurrentTarget;
        if (comp != default && actor == comp)
        {
            var damagedCells = Module.FindComponent<ArenaChanges>()!.DamagedCells;
            var tiles = ArenaChanges.Tiles;

            foreach (var index in damagedCells.SetBits())
            {
                var tile = tiles[index];
                yield return new AOEInstance(Mouser.Rect, tile.Center, Color: Colors.FutureVulnerable, Risky: false);
            }
        }
    }
}
