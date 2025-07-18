namespace BossMod.Endwalker.VariantCriterion.V02MR.V025Enenra;

sealed class ArenaChange(BossModule module) : Components.GenericAOEs(module)
{
    private static readonly AOEShapeDonut donut = new(20f, 21f);
    private AOEInstance? _aoe;

    public override ReadOnlySpan<AOEInstance> ActiveAOEs(int slot, Actor actor) => Utils.ZeroOrOne(ref _aoe);

    public override void OnCastStarted(Actor caster, ActorCastInfo spell)
    {
        if (spell.Action.ID == (uint)AID.FlagrantCombustion && Arena.Bounds == V025Enenra.StartingBounds)
        {
            _aoe = new(donut, Arena.Center, default, Module.CastFinishAt(spell, 2.9d));
        }
    }

    public override void OnEventEnvControl(byte index, uint state)
    {
        if (index == 0x28u && state == 0x00020001u)
        {
            Arena.Bounds = V025Enenra.DefaultBounds;
            _aoe = null;
        }
    }
}
