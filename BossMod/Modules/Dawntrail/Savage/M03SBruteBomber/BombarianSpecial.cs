﻿namespace BossMod.Dawntrail.Savage.M03SBruteBomber;

class BombarianSpecial(BossModule module) : Components.UniformStackSpread(module, 5, 5, 2, 2, alwaysShowSpreads: true)
{
    public enum Mechanic { None, Spread, Pairs }

    public Mechanic CurMechanic;

    public void Show(float delay)
    {
        switch (CurMechanic)
        {
            case Mechanic.Spread:
                AddSpreads(Raid.WithoutSlot(true, true, true), WorldState.FutureTime(delay));
                break;
            case Mechanic.Pairs:
                // TODO: can target any role
                AddStacks(Raid.WithoutSlot(true, true, true).Where(p => p.Class.IsSupport()), WorldState.FutureTime(delay));
                break;
        }
    }

    public override void AddGlobalHints(GlobalHints hints)
    {
        if (CurMechanic != Mechanic.None)
            hints.Add(CurMechanic.ToString());
    }

    public override void OnCastStarted(Actor caster, ActorCastInfo spell)
    {
        var mechanic = (AID)spell.Action.ID switch
        {
            AID.OctoboomBombarianSpecial => Mechanic.Spread,
            AID.QuadroboomBombarianSpecial => Mechanic.Pairs,
            _ => Mechanic.None
        };
        if (mechanic != Mechanic.None)
            CurMechanic = mechanic;
    }

    public override void OnEventCast(Actor caster, ActorCastEvent spell)
    {
        if ((AID)spell.Action.ID is AID.BombariboomSpread or AID.BombariboomPair)
        {
            Spreads.Clear();
            Stacks.Clear();
            CurMechanic = Mechanic.None;
        }
    }
}

class BombarianSpecialRaidwide(BossModule module) : Components.CastCounterMulti(module, [(uint)AID.BombarianSpecialRaidwide1,
(uint)AID.BombarianSpecialRaidwide2, (uint)AID.BombarianSpecialRaidwide3, (uint)AID.BombarianSpecialRaidwide4,
(uint)AID.BombarianSpecialRaidwide5, (uint)AID.BombarianSpecialRaidwide6, (uint)AID.SpecialBombarianSpecialRaidwide1,
(uint)AID.SpecialBombarianSpecialRaidwide2, (uint)AID.SpecialBombarianSpecialRaidwide3,
(uint)AID.SpecialBombarianSpecialRaidwide4, (uint)AID.SpecialBombarianSpecialRaidwide5, (uint)AID.SpecialBombarianSpecialRaidwide6]);

abstract class SpecialOut(BossModule module, uint aid) : Components.SimpleAOEs(module, aid, 10);
class BombarianSpecialOut(BossModule module) : SpecialOut(module, (uint)AID.BombarianSpecialOut);
class SpecialBombarianSpecialOut(BossModule module) : SpecialOut(module, (uint)AID.SpecialBombarianSpecialOut);

abstract class SpecialIn(BossModule module, uint aid) : Components.SimpleAOEs(module, aid, new AOEShapeDonut(6, 40));
class BombarianSpecialIn(BossModule module) : SpecialIn(module, (uint)AID.BombarianSpecialIn);
class SpecialBombarianSpecialIn(BossModule module) : SpecialIn(module, (uint)AID.SpecialBombarianSpecialIn);

class BombarianSpecialAOE(BossModule module) : Components.SimpleAOEs(module, (uint)AID.BombarianSpecialAOE, 8);
class BombarianSpecialKnockback(BossModule module) : Components.SimpleKnockbacks(module, (uint)AID.BombarianSpecialKnockback, 10);
