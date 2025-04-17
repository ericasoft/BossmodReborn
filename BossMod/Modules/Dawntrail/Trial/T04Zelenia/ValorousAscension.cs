namespace BossMod.Dawntrail.Trial.T04Zelenia;

class ValorousAscension(BossModule module) : Components.RaidwideCast(module, (uint)AID.ValorousAscension1, "Raidwide x3");

class ValorousAscensionRect(BossModule module) : Components.GenericAOEs(module)
{
    private static readonly AOEShapeRect rect = new(40f, 4f);
    private readonly List<AOEInstance> _aoes = new(4);

    public override ReadOnlySpan<AOEInstance> ActiveAOEs(int slot, Actor actor) => _aoes.Count > 1 ? CollectionsMarshal.AsSpan(_aoes)[..2] : [];

    public override void OnActorPlayActionTimelineEvent(Actor actor, ushort id)
    {
        if (actor.OID == (uint)OID.BriarThorn1 && id == 0x11DBu)
            _aoes.Add(new(rect, WPos.ClampToGrid(actor.Position), actor.Rotation, WorldState.FutureTime(11.1d)));
    }

    public override void OnCastFinished(Actor caster, ActorCastInfo spell)
    {
        if (_aoes.Count != 0 && spell.Action.ID == (uint)AID.ValorousAscensionRect)
        {
            _aoes.RemoveAt(0);
        }
    }
}
