namespace BossMod.Shadowbringers.Dungeon.D08AkadaemiaAnyder.D082MorbolMarquis;

public enum OID : uint
{
    Boss = 0x249E, // R5.5
    Voidzone = 0x1EA1A1, // R2.0
    Helper = 0x233C
}

public enum AID : uint
{
    AutoAttack = 872, // Boss->player, no cast, single-target

    SapShowerVisual = 15892, // Boss->self, no cast, single-target, spread
    SapShower = 15893, // Helper->player, 5.0s cast, range 8 circle

    Lash = 15894, // Boss->players, no cast, single-target, double hit tankbuster, after every putrid breath and every sap shower

    ArborStorm = 15895, // Boss->self, 3.0s cast, range 50 circle, raidwide

    ExtensibleTendrilsFirst = 15888, // Boss->self, 5.0s cast, range 25 width 6 cross, 5 hits, sort of a rotation, except sometimes the boss seems to hit the same spot multiple times (random?)
    ExtensibleTendrilsRest = 15889, // Boss->self, no cast, range 25 width 6 cross

    PutridBreath = 15890, // Boss->self, no cast, range 25 90-degree cone, after 5 hits of Extensible Tendrils
    Blossom = 15891 // Boss->self, 4.0s cast, single-target
}

class ArborStorm(BossModule module) : Components.RaidwideCast(module, (uint)AID.ArborStorm);

abstract class Leash(BossModule module, uint aid) : Components.SingleTargetEventDelay(module, aid, (uint)AID.Lash, 3.4f)  // actual delay can be higher since boss needs to run into melee range for it
{
    public override void OnCastStarted(Actor caster, ActorCastInfo spell)
    {
        if (Targets.Count != 0 && spell.Action.ID != ActionVisual && spell.Action.ID != ActionVisual)  // it seems like sometimes the tankbuster gets skipped and it does it twice next time
            Targets.Clear();
    }
}

class LeashSapShower(BossModule module) : Leash(module, (uint)AID.SapShowerVisual);
class LeashPutridBreath(BossModule module) : Leash(module, (uint)AID.PutridBreath);

class SapShower(BossModule module) : Components.SpreadFromCastTargets(module, (uint)AID.SapShower, 8);

class ExtensibleTendrilsPutridBreath(BossModule module) : Components.GenericAOEs(module)
{
    private static readonly AOEShapeCross cross = new(25f, 3f);
    private static readonly Angle a45 = 45f.Degrees();
    private static readonly AOEShapeCone cone = new(25f, a45);
    private AOEInstance? _aoe;
    private DateTime activation;
    private int remainingCasts;

    public override ReadOnlySpan<AOEInstance> ActiveAOEs(int slot, Actor actor)
    {
        if (_aoe is AOEInstance aoe)
        {
            return new AOEInstance[1] { aoe };
        }
        var aoes = new List<AOEInstance>(2);
        if (remainingCasts > 0)
        {
            var delay1 = activation.AddSeconds((5d - remainingCasts) * 6.1d);
            if ((delay1 - WorldState.CurrentTime).TotalSeconds <= 2.5d)
                aoes.Add(new(cross, Module.PrimaryActor.Position.Quantized(), Module.PrimaryActor.Rotation + a45, delay1));
        }
        var delay2 = activation.AddSeconds(27.1d);
        if (activation != default && (delay2 - WorldState.CurrentTime).TotalSeconds <= 4.9f)
            aoes.Add(new(cone, Module.PrimaryActor.Position.Quantized(), Module.PrimaryActor.Rotation, delay2));
        return CollectionsMarshal.AsSpan(aoes);
    }

    public override void OnCastStarted(Actor caster, ActorCastInfo spell)
    {
        if (spell.Action.ID == (uint)AID.ExtensibleTendrilsFirst)
        {
            remainingCasts = 5;
            activation = Module.CastFinishAt(spell);
            _aoe = new(cross, spell.LocXZ, spell.Rotation, activation);
        }
    }

    public override void OnEventCast(Actor caster, ActorCastEvent spell)
    {
        switch (spell.Action.ID)
        {
            case (uint)AID.ExtensibleTendrilsFirst:
            case (uint)AID.ExtensibleTendrilsRest:
                _aoe = null;
                --remainingCasts;
                break;

            case (uint)AID.PutridBreath:
                activation = default;
                break;
        }
    }
}

class BlossomArenaChanges(BossModule module) : BossComponent(module)
{
    public override void OnActorEAnim(Actor actor, uint state)
    {
        if (actor.OID == (uint)OID.Voidzone)
        {
            Arena.Bounds = state switch
            {
                0x00100020u => D082MorbolMarquis.YellowBlossomBounds,
                0x00010002u => D082MorbolMarquis.BlueBlossomBounds,
                _ => D082MorbolMarquis.DefaultBounds
            };
        }
    }
}

class D082MorbolMarquisStates : StateMachineBuilder
{
    public D082MorbolMarquisStates(BossModule module) : base(module)
    {
        TrivialPhase()
            .ActivateOnEnter<BlossomArenaChanges>()
            .ActivateOnEnter<ArborStorm>()
            .ActivateOnEnter<LeashSapShower>()
            .ActivateOnEnter<LeashPutridBreath>()
            .ActivateOnEnter<SapShower>()
            .ActivateOnEnter<ExtensibleTendrilsPutridBreath>();
    }
}

[ModuleInfo(BossModuleInfo.Maturity.Verified, Contributors = "The Combat Reborn Team (Malediktus)", GroupType = BossModuleInfo.GroupType.CFC, GroupID = 661, NameID = 8272)]
public class D082MorbolMarquis(WorldState ws, Actor primary) : BossModule(ws, primary, DefaultBounds.Center, DefaultBounds)
{
    private const float X = -224f, InnerRadius = 10f, OuterRadius = 15f, Radius = 25f;
    private const int Edges = 12;
    private static readonly WPos arenaCenter = new(X, -38f);
    private static readonly Angle a45 = 45f.Degrees(), a135 = 135f.Degrees();
    private static readonly Polygon[] defaultCircle = [new(arenaCenter, 24.5f * CosPI.Pi48th, 48)];
    private static readonly Rectangle[] defaultDifference = [new(new(X, -13f), Radius, 1.1f), new(new(X, -63f), Radius, 1.1f)];
    private static readonly Shape[] blueBlossom = [new ConeV(arenaCenter, InnerRadius, a45, a45, Edges), new ConeV(arenaCenter, InnerRadius, -a135, a45, Edges),
    new DonutSegmentV(arenaCenter, OuterRadius, Radius, a45, a45, Edges), new DonutSegmentV(arenaCenter, OuterRadius, Radius, -a135, a45, Edges)];
    private static readonly Shape[] yellowBlossom = [new ConeV(arenaCenter, InnerRadius, -a45, a45, Edges), new ConeV(arenaCenter, InnerRadius, a135, a45, Edges),
    new DonutSegmentV(arenaCenter, OuterRadius, Radius, -a45, a45, Edges), new DonutSegmentV(arenaCenter, OuterRadius, Radius, a135, a45, Edges)];
    public static readonly ArenaBoundsComplex DefaultBounds = new(defaultCircle, defaultDifference);
    public static readonly ArenaBoundsComplex BlueBlossomBounds = new(defaultCircle, [.. defaultDifference, .. blueBlossom]);
    public static readonly ArenaBoundsComplex YellowBlossomBounds = new(defaultCircle, [.. defaultDifference, .. yellowBlossom]);
}
