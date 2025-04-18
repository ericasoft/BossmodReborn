﻿namespace BossMod.Endwalker.Savage.P11SThemis;

class JuryOverrulingProtean(BossModule module) : Components.BaitAwayEveryone(module, module.PrimaryActor, new AOEShapeRect(50, 4))
{
    public override void OnEventCast(Actor caster, ActorCastEvent spell)
    {
        if ((AID)spell.Action.ID is AID.JuryOverrulingProteanLight or AID.JuryOverrulingProteanDark)
            ++NumCasts;
    }
}

class IllusoryGlare(BossModule module) : Components.SimpleAOEs(module, (uint)AID.IllusoryGlare, 5);
class IllusoryGloom(BossModule module) : Components.SimpleAOEs(module, (uint)AID.IllusoryGloom, new AOEShapeDonut(2, 9));
