﻿namespace BossMod.Dawntrail.Extreme.Ex1Valigarmanda;

class VolcanicDrop(BossModule module) : Components.GenericAOEs(module, (uint)AID.VolcanicDropAOE)
{
    public AOEInstance? AOE;

    private static readonly AOEShapeCircle _shape = new(20);

    public override ReadOnlySpan<AOEInstance> ActiveAOEs(int slot, Actor actor) => Utils.ZeroOrOne(ref AOE);

    public override void OnEventEnvControl(byte index, uint state)
    {
        // state transitions:
        // 00020001 - both volcanos, appear after skyruin end
        // 00200010 - active volcano, eruption start after triscourge end
        // 00800040 - active volcano, some eruption animation
        // 02000100 - active volcano, eruption end after puddles
        if (index is 14 or 15 && state == 0x00200010)
        {
            AOE = new(_shape, Arena.Center + new WDir(index == 14 ? 13 : -13, 0), default, WorldState.FutureTime(7.8f));
        }
    }
}

class VolcanicDropPuddle(BossModule module) : Components.SimpleAOEs(module, (uint)AID.VolcanicDropPuddle, 2);
