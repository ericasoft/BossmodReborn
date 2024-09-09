namespace BossMod.Endwalker.VariantCriterion.V02MR.V025Enenra;

class PipeCleaner(BossModule module) : Components.BaitAwayTethers(module, new AOEShapeRect(60, 5), (uint)TetherID.PipeCleaner);
class Uplift(BossModule module) : Components.LocationTargetedAOEs(module, ActionID.MakeSpell(AID.Uplift), 6);
class Snuff(BossModule module) : Components.BaitAwayCast(module, ActionID.MakeSpell(AID.Snuff), new AOEShapeCircle(6), true)
{
    public override void AddGlobalHints(GlobalHints hints)
    {
        if (CurrentBaits.Count > 0)
            hints.Add("Tankbuster cleave");
    }
}

class Smoldering(BossModule module) : Components.SelfTargetedAOEs(module, ActionID.MakeSpell(AID.Smoldering), new AOEShapeCircle(8), 8);
class FlagrantCombustion(BossModule module) : Components.RaidwideCast(module, ActionID.MakeSpell(AID.FlagrantCombustion));
class SmokeRings(BossModule module) : Components.SelfTargetedAOEs(module, ActionID.MakeSpell(AID.SmokeRings), new AOEShapeCircle(16));
class ClearingSmoke(BossModule module) : Components.KnockbackFromCastTarget(module, ActionID.MakeSpell(AID.ClearingSmoke), 16, stopAfterWall: true)
{
    private DateTime activation;

    public override void OnCastStarted(Actor caster, ActorCastInfo spell)
    {
        base.OnCastStarted(caster, spell);
        if ((AID)spell.Action.ID == AID.ClearingSmoke)
            activation = Module.CastFinishAt(spell, 0.4f);
    }

    public override void AddAIHints(int slot, Actor actor, PartyRolesConfig.Assignment assignment, AIHints hints)
    {
        var forbidden = new List<Func<WPos, float>>();
        var component = Module.FindComponent<Smoldering>()?.ActiveAOEs(slot, actor)?.ToList();
        if (component?.Count == 0 && (Sources(slot, actor).Any() || activation > WorldState.CurrentTime)) // 0.4s delay to wait for action effect
            hints.AddForbiddenZone(ShapeDistance.InvertedCircle(Arena.Center, 4), activation.AddSeconds(-0.4f));
    }
}

class StringRock(BossModule module) : Components.ConcentricAOEs(module, _shapes)
{
    private static readonly AOEShape[] _shapes = [new AOEShapeCircle(6), new AOEShapeDonut(6, 12), new AOEShapeDonut(12, 18), new AOEShapeDonut(18, 24)];

    public override void OnCastStarted(Actor caster, ActorCastInfo spell)
    {
        if ((AID)spell.Action.ID == AID.KiseruClamor)
            AddSequence(spell.LocXZ, Module.CastFinishAt(spell));
    }

    public override void OnCastFinished(Actor caster, ActorCastInfo spell)
    {
        if (Sequences.Count > 0)
        {
            var order = (AID)spell.Action.ID switch
            {
                AID.KiseruClamor => 0,
                AID.BedrockUplift1 => 1,
                AID.BedrockUplift2 => 2,
                AID.BedrockUplift3 => 3,
                _ => -1
            };
            AdvanceSequence(order, spell.TargetID == caster.InstanceID ? caster.Position : WorldState.Actors.Find(spell.TargetID)?.Position ?? spell.LocXZ, WorldState.FutureTime(2));
        }
    }
}

[ModuleInfo(BossModuleInfo.Maturity.Verified, Contributors = "The Combat Reborn Team (Malediktus, LTS)", GroupType = BossModuleInfo.GroupType.CFC, GroupID = 945, NameID = 12393, SortOrder = 6)]
public class V025Enenra(WorldState ws, Actor primary) : BossModule(ws, primary, new(900, -900), StartingBounds)
{
    public static readonly ArenaBoundsCircle StartingBounds = new(20.5f);
    public static readonly ArenaBoundsCircle DefaultBounds = new(20);

    protected override void DrawEnemies(int pcSlot, Actor pc)
    {
        Arena.Actor(PrimaryActor);
        Arena.Actors(Enemies(OID.EnenraClone));
    }
}
