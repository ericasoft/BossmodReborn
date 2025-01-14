﻿namespace BossMod.Dawntrail.Ultimate.FRU;

class P5PolarizingStrikes(BossModule module) : Components.GenericAOEs(module)
{
    private readonly List<AOEInstance> _aoes = []; // 'afterglow'
    private readonly Actor?[] _baiters = [null, null]; // light/left, dark/right
    private readonly BitMask[] _forbidden = [default, default];
    private Actor? _source;

    private static readonly AOEShapeRect _shape = new(100, 3);

    public override IEnumerable<AOEInstance> ActiveAOEs(int slot, Actor actor) => _aoes;

    public override void Update()
    {
        _baiters[0] = _baiters[1] = null;
        if (_source != null && _aoes.Count == 0)
        {
            var left = _source.Rotation.ToDirection().OrthoL();
            float distL = float.MaxValue, distR = float.MaxValue;
            foreach (var p in Raid.WithoutSlot())
            {
                var off = p.Position - _source.Position;
                var side = left.Dot(off) > 0;
                ref var target = ref _baiters[side ? 0 : 1];
                ref var dist = ref side ? ref distL : ref distR;
                var d = off.LengthSq();
                if (d < dist)
                {
                    dist = d;
                    target = p;
                }
            }
        }
    }

    public override void AddHints(int slot, Actor actor, TextHints hints)
    {
        if (_source != null && _aoes.Count == 0)
        {
            if (_baiters.Contains(actor) && (_forbidden[0] | _forbidden[1])[slot])
                hints.Add("Hide behind party!");

            var inLight = InAOE(_source, _baiters[0], actor);
            var inDark = InAOE(_source, _baiters[1], actor);
            if (inLight == inDark)
                hints.Add("Stay in group!");
            else if (_forbidden[inLight ? 0 : 1][slot])
                hints.Add("Go to correct group!");
        }
        else
        {
            base.AddHints(slot, actor, hints);
        }
    }

    public override void DrawArenaForeground(int pcSlot, Actor pc)
    {
        if (_source == null)
            return;
        foreach (var (baiter, forbidden) in _baiters.Zip(_forbidden))
            if (baiter != null)
                _shape.Outline(Arena, _source.Position, Angle.FromDirection(baiter.Position - _source.Position), forbidden[pcSlot] ? ArenaColor.Danger : ArenaColor.Safe);
    }

    public override void OnCastStarted(Actor caster, ActorCastInfo spell)
    {
        switch ((AID)spell.Action.ID)
        {
            case AID.PolarizingStrikes:
            case AID.PolarizingPaths:
                _source = caster;
                break;
            case AID.CruelPathOfLightBait:
            case AID.CruelPathOfDarknessBait:
                _source = null;
                break;
        }
    }

    public override void OnEventCast(Actor caster, ActorCastEvent spell)
    {
        switch ((AID)spell.Action.ID)
        {
            case AID.CruelPathOfLightBait:
            case AID.CruelPathOfDarknessBait:
                ++NumCasts;
                _aoes.Add(new(_shape, caster.Position, spell.Rotation, WorldState.FutureTime(2)));
                break;
            case AID.CruelPathOfLightAOE:
            case AID.CruelPathOfDarknessAOE:
                ++NumCasts;
                _aoes.Clear();
                break;
        }
    }

    public override void OnStatusGain(Actor actor, ActorStatus status)
    {
        switch ((SID)status.ID)
        {
            case SID.LightResistanceDown:
                _forbidden[0].Set(Raid.FindSlot(actor.InstanceID));
                break;
            case SID.DarkResistanceDown:
                _forbidden[1].Set(Raid.FindSlot(actor.InstanceID));
                break;
        }
    }

    private bool InAOE(Actor source, Actor? target, Actor player) => target != null && (target == player || _shape.Check(player.Position, source.Position, Angle.FromDirection(target.Position - source.Position)));
}
