﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace BossMod.Endwalker.Unreal.Un2Sephirot
{
    class P1TripleTrial : Components.Cleave
    {
        public P1TripleTrial() : base(ActionID.MakeSpell(AID.TripleTrial), new AOEShapeCone(18.5f, 45.Degrees())) { } // TODO: verify angle
    }

    class P1Ein : Components.SelfTargetedAOEs
    {
        public P1Ein() : base(ActionID.MakeSpell(AID.Ein), new AOEShapeRect(50, 22.5f)) { }
    }

    class P2GenesisCochma : Components.CastCounter
    {
        public P2GenesisCochma() : base(ActionID.MakeSpell(AID.GenesisCochma)) { }
    }

    class P2GenesisBinah : Components.CastCounter
    {
        public P2GenesisBinah() : base(ActionID.MakeSpell(AID.GenesisBinah)) { }
    }

    class P3EinSofOhr : Components.CastCounter
    {
        public P3EinSofOhr() : base(ActionID.MakeSpell(AID.EinSofOhr)) { }
    }

    class P3Yesod : Components.SelfTargetedAOEs
    {
        public P3Yesod() : base(ActionID.MakeSpell(AID.Yesod), new AOEShapeCircle(4)) { }
    }

    [ModuleInfo(PrimaryActorOID = (uint)OID.BossP1)]
    public class Un2Sephirot : BossModule
    {
        public Actor? BossP1() => PrimaryActor.IsDestroyed ? null : PrimaryActor;

        private Actor? _bossP3;
        public Actor? BossP3() => _bossP3;

        public Un2Sephirot(WorldState ws, Actor primary) : base(ws, primary, new ArenaBoundsCircle(new(0, 0), 20)) { }

        protected override void UpdateModule()
        {
            // TODO: this is an ugly hack, think how multi-actor fights can be implemented without it...
            // the problem is that on wipe, any actor can be deleted and recreated in the same frame
            _bossP3 ??= StateMachine.ActivePhaseIndex >= 0 ? Enemies(OID.BossP3).FirstOrDefault() : null;
        }

        protected override void DrawEnemies(int pcSlot, Actor pc)
        {
            if (StateMachine.ActivePhaseIndex <= 0)
                Arena.Actor(PrimaryActor, ArenaColor.Enemy);
            else if (StateMachine.ActivePhaseIndex == 2)
                Arena.Actor(_bossP3, ArenaColor.Enemy);
        }
    }
}
