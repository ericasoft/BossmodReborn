﻿namespace BossMod.Dawntrail.Extreme.Ex3QueenEternal;

class ProsecutionOfWar(BossModule module) : Components.TankSwap(module, ActionID.MakeSpell(AID.ProsecutionOfWar), ActionID.MakeSpell(AID.ProsecutionOfWar), ActionID.MakeSpell(AID.ProsecutionOfWarAOE), 3.1f, null, true);
class DyingMemory(BossModule module) : Components.CastCounter(module, ActionID.MakeSpell(AID.DyingMemory));
class DyingMemoryLast(BossModule module) : Components.CastCounter(module, ActionID.MakeSpell(AID.DyingMemoryLast));

[ModuleInfo(BossModuleInfo.Maturity.Contributed, Contributors = "veyn, Malediktus", PlanLevel = 100, PrimaryActorOID = (uint)OID.BossP1, GroupType = BossModuleInfo.GroupType.CFC, GroupID = 1017, NameID = 13029)]
public class Ex3QueenEternal(WorldState ws, Actor primary) : BossModule(ws, primary, ArenaCenter, NormalBounds)
{
    public static readonly WPos ArenaCenter = Trial.T03QueenEternal.T03QueenEternal.ArenaCenter;
    public static readonly ArenaBoundsSquare NormalBounds = Trial.T03QueenEternal.T03QueenEternal.DefaultBounds;
    public static readonly ArenaBoundsComplex WindBounds = Trial.T03QueenEternal.T03QueenEternal.XArena;
    public static readonly ArenaBoundsComplex EarthBounds = Trial.T03QueenEternal.T03QueenEternal.SplitArena;
    private static readonly Rectangle[] iceRects = [new Rectangle(new(112, 95), 4, 15), new Rectangle(new(88, 95), 4, 15), new Rectangle(ArenaCenter, 2, 10)];
    public static readonly Rectangle[] IceRectsAll = [.. iceRects, new Rectangle(new(100, 96), 8, 2), new Rectangle(new(100, 104), 8, 2)];
    public static readonly ArenaBoundsComplex IceBounds = new(iceRects, Offset: Trial.T03QueenEternal.T03QueenEternal.OffSet);
    public static readonly ArenaBoundsComplex IceBridgeBounds = new(IceRectsAll, Offset: Trial.T03QueenEternal.T03QueenEternal.OffSet);

    private Actor? _bossP2;
    public Actor? BossP1() => PrimaryActor;
    public Actor? BossP2() => _bossP2;

    protected override void UpdateModule()
    {
        // TODO: this is an ugly hack, think how multi-actor fights can be implemented without it...
        // the problem is that on wipe, any actor can be deleted and recreated in the same frame
        _bossP2 ??= StateMachine.ActivePhaseIndex > 0 ? Enemies(OID.BossP2).FirstOrDefault() : null;
    }

    protected override void DrawEnemies(int pcSlot, Actor pc)
    {
        Arena.Actor(PrimaryActor);
        Arena.Actor(_bossP2);
    }
}
