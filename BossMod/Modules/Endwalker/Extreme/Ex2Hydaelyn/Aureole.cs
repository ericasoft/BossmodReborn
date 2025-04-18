﻿namespace BossMod.Endwalker.Extreme.Ex2Hydaelyn;

abstract class Aureoles(BossModule module, uint aid) : Components.SimpleAOEs(module, aid, new AOEShapeCone(40, 75.Degrees()));
class LateralAureole1(BossModule module) : Aureoles(module, (uint)AID.LateralAureole1AOE);
class LateralAureole2(BossModule module) : Aureoles(module, (uint)AID.LateralAureole2AOE);
class Aureole1(BossModule module) : Aureoles(module, (uint)AID.Aureole1AOE);
class Aureole2(BossModule module) : Aureoles(module, (uint)AID.Aureole2AOE);

// component tracking [lateral] aureole mechanic, only exists for the timeline anymore
class Aureole(BossModule module) : BossComponent(module)
{
    public bool Done;

    public override void OnEventCast(Actor caster, ActorCastEvent spell)
    {
        if ((AID)spell.Action.ID is AID.Aureole1AOE or AID.Aureole2AOE or AID.LateralAureole1AOE or AID.LateralAureole2AOE)
            Done = true;
    }
}
