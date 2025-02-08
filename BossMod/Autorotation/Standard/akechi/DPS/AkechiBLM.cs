﻿using static BossMod.AIHints;
using FFXIVClientStructs.FFXIV.Client.Game.Gauge;
using BossMod.BLM;

namespace BossMod.Autorotation.akechi;
//Contribution by Akechi
//Discord: @akechdz or 'Akechi' on Puni.sh for maintenance

public sealed class AkechiBLM(RotationModuleManager manager, Actor player) : AkechiTools<AID, TraitID>(manager, player)
{
    #region Enums: Abilities / Strategies
    public enum Track
    {
        AOE,                 //ST&AOE rotations tracking
        Movement,            //Movement strategy tracking
        Thunder,             //Thunder tracking
        Polyglot,            //Polyglot tracking
        Manafont,            //Manafont tracking
        Triplecast,          //Triplecast tracking
        LeyLines,            //Ley Lines tracking
        Potion,              //Potion item tracking
        TPUS,                //Transpose&UmbralSoul combo tracking
        Casting,              //Cast while Moving option tracking
        Transpose,           //Transpose tracking
        Amplifier,           //Amplifier tracking
        Retrace,             //Retrace tracking
        BTL,                 //Between the Lines tracking
    }
    public enum AOEStrategy
    {
        Auto,                //Automatically decide when to use ST or AOE rotation based on targets nearby
        ForceST,             //Force ST rotation only
        ForceAOE,            //Force AOE rotation only
    }
    public enum MovementStrategy
    {
        Allow,               //Allow the use of all abilities for movement, regardless of any setting or condition set by the user in other options
        AllowNoScathe,       //Allow the use of all abilities for movement, except Scathe
        OnlyGCDs,            //Only use instant cast GCDs for movement (Polyglots->Firestarter->Thunder->Scathe if nothing left), regardless of any setting or condition set by the user in other options
        OnlyOGCDs,           //Only use OGCDs for movement, (Swiftcast->Triplecast) regardless of any setting or condition set by the user in other options
        OnlyScathe,          //Only use Scathe for movement
        Forbid               //Forbid the use of any abilities for movement
    }
    public enum ThunderStrategy
    {
        Thunder3,            //Force use of Thunder if target has 3s or less remaining on DOT effect
        Thunder6,            //Force use of Thunder if target has 6s or less remaining on DOT effect
        Thunder9,            //Force use of Thunder if target has 9s or less remaining on DOT effect
        Thunder0,            //Force use of Thunder if target has does not have DOT effect
        Force,               //Force use of Thunder regardless of DOT effect
        Delay                //Delay the use of Thunder for manual or strategic usage
    }
    public enum PolyglotStrategy
    {
        AutoSpendAll,        //Spend all Polyglots as soon as possible
        AutoHold1,           //Spend 2 Polyglots; holds one for manual usage
        AutoHold2,           //Spend 1 Polyglot; holds two for manual usage
        AutoHold3,           //Holds all Polyglots for as long as possible
        XenoSpendAll,        //Use Xenoglossy as optimal spender, regardless of targets nearby; spends all Polyglots
        XenoHold1,           //Use Xenoglossy as optimal spender, regardless of targets nearby; holds one Polyglot for manual usage
        XenoHold2,           //Use Xenoglossy as optimal spender, regardless of targets nearby; holds two Polyglots for manual usage
        XenoHold3,           //Holds all Polyglots for as long as possible
        FoulSpendAll,        //Use Foul as optimal spender, regardless of targets nearby
        FoulHold1,           //Use Foul as optimal spender, regardless of targets nearby; holds one Polyglot for manual usage
        FoulHold2,           //Use Foul as optimal spender, regardless of targets nearby; holds two Polyglots for manual usage
        FoulHold3,           //Holds all Polyglots for as long as possible
        ForceXeno,           //Force use of Xenoglossy
        ForceFoul,           //Force use of Foul
        Delay                //Delay the use of Polyglot abilities for manual or strategic usage
    }
    public enum ManafontStrategy
    {
        Automatic,           //Automatically decide when to use Manafont
        Force,               //Force the use of Manafont (180s TotalCD), regardless of weaving conditions
        ForceWeave,          //Force the use of Manafont (180s TotalCD) in any next possible weave slot
        ForceEX,             //Force the use of Manafont (100s TotalCD), regardless of weaving conditions
        ForceWeaveEX,        //Force the use of Manafont (100s TotalCD) in any next possible weave slot
        Delay                //Delay the use of Manafont for strategic reasons
    }
    public enum TriplecastStrategy
    {
        Automatic,           //Automatically decide when to use Triplecast
        Force,               //Force the use of Triplecast; use all charges
        Force1,              //Force the use of Triplecast; holds one charge for manual usage
        ForceWeave,          //Force the use of Triplecast in any next possible weave slot
        ForceWeave1,         //Force the use of Triplecast in any next possible weave slot; holds one charge for manual usage
        Delay                //Delay the use of Triplecast
    }
    public enum LeyLinesStrategy
    {
        Automatic,           //Automatically decide when to use Ley Lines
        Force,               //Force the use of Ley Lines, regardless of weaving conditions
        Force1,              //Force the use of Ley Lines; holds one charge for manual usage
        ForceWeave,          //Force the use of Ley Lines in any next possible weave slot
        ForceWeave1,         //Force the use of Ley Lines in any next possible weave slot; holds one charge for manual usage
        Delay                //Delay the use of Ley Lines
    }
    public enum PotionStrategy
    {
        Manual,              //Manual potion usage
        AlignWithRaidBuffs,  //Align potion usage with raid buffs
        Immediate            //Use potions immediately when available
    }
    public enum TPUSStrategy
    {
        Allow,               //Allow Transpose & Umbral Soul combo whenever available
        OOConly,             //Only use Transpose & Umbral Soul combo when fully out of combat
        Forbid               //Forbid Transpose & Umbral Soul combo
    }
    public enum CastingOption
    {
        Allow,               //Allow casting while moving
        Forbid               //Forbid casting while moving
    }
    public enum OffensiveStrategy
    {
        Automatic,           //Automatically decide when to use off-global offensive abilities
        Force,               //Force the use of off-global offensive abilities, regardless of weaving conditions
        AnyWeave,            //Force the use of off-global offensive abilities in any next possible weave slot
        EarlyWeave,          //Force the use of off-global offensive abilities in very next FIRST weave slot only
        LateWeave,           //Force the use of off-global offensive abilities in very next LAST weave slot only
        Delay                //Delay the use of offensive abilities for strategic reasons
    }
    #endregion

    #region Module Definitions
    public static RotationModuleDefinition Definition()
    {
        var res = new RotationModuleDefinition("Akechi BLM", //Title
            "Standard Rotation Module", //Description
            "Standard rotation (Akechi)|DPS", //Category
            "Akechi", //Contributor
            RotationModuleQuality.Ok, //Quality
            BitMask.Build(Class.THM, Class.BLM), //Job
            100); //Level supported

        res.DefineShared();
        res.Define(Track.Movement).As<MovementStrategy>("Movement", uiPriority: 195)
            .AddOption(MovementStrategy.Allow, "Allow", "Allow the use of all appropriate abilities for movement")
            .AddOption(MovementStrategy.AllowNoScathe, "AllowNoScathe", "Allow the use of all appropriate abilities for movement except for Scathe")
            .AddOption(MovementStrategy.OnlyGCDs, "OnlyGCDs", "Only use instant cast GCDs for movement; Polyglots->Firestarter->Thunder->Scathe if nothing left")
            .AddOption(MovementStrategy.OnlyOGCDs, "OnlyOGCDs", "Only use OGCDs for movement; Swiftcast->Triplecast")
            .AddOption(MovementStrategy.OnlyScathe, "OnlyScathe", "Only use Scathe for movement")
            .AddOption(MovementStrategy.Forbid, "Forbid", "Forbid the use of any abilities for movement");
        res.Define(Track.Thunder).As<ThunderStrategy>("Thunder", "DOT", uiPriority: 190)
            .AddOption(ThunderStrategy.Thunder3, "Thunder3", "Use Thunder if target has 3s or less remaining on DoT effect", 0, 27, ActionTargets.Hostile, 6)
            .AddOption(ThunderStrategy.Thunder6, "Thunder6", "Use Thunder if target has 6s or less remaining on DoT effect", 0, 27, ActionTargets.Hostile, 6)
            .AddOption(ThunderStrategy.Thunder9, "Thunder9", "Use Thunder if target has 9s or less remaining on DoT effect", 0, 27, ActionTargets.Hostile, 6)
            .AddOption(ThunderStrategy.Thunder0, "Thunder0", "Use Thunder if target does not have DoT effect", 0, 27, ActionTargets.Hostile, 6)
            .AddOption(ThunderStrategy.Force, "Force", "Force use of Thunder regardless of DoT effect", 0, 27, ActionTargets.Hostile, 6)
            .AddOption(ThunderStrategy.Delay, "Delay", "Delay the use of Thunder for manual or strategic usage", 0, 0, ActionTargets.Hostile, 6)
            .AddAssociatedActions(AID.Thunder1, AID.Thunder2, AID.Thunder3, AID.Thunder4, AID.HighThunder, AID.HighThunder2);
        res.Define(Track.Polyglot).As<PolyglotStrategy>("Polyglot", "Polyglot", uiPriority: 180)
            .AddOption(PolyglotStrategy.AutoSpendAll, "AutoSpendAll", "Spend all Polyglots as soon as possible", 0, 0, ActionTargets.Hostile, 70)
            .AddOption(PolyglotStrategy.AutoHold1, "AutoHold1", "Spend 2 Polyglots; holds one for manual usage", 0, 0, ActionTargets.Hostile, 70)
            .AddOption(PolyglotStrategy.AutoHold2, "AutoHold2", "Spend 1 Polyglot; holds two for manual usage", 0, 0, ActionTargets.Hostile, 70)
            .AddOption(PolyglotStrategy.AutoHold3, "AutoHold3", "Holds all Polyglots for as long as possible", 0, 0, ActionTargets.Hostile, 70)
            .AddOption(PolyglotStrategy.XenoSpendAll, "XenoSpendAll", "Use Xenoglossy as optimal spender, regardless of targets nearby; spends all Polyglots", 0, 0, ActionTargets.Hostile, 80)
            .AddOption(PolyglotStrategy.XenoHold1, "XenoHold1", "Use Xenoglossy as optimal spender, regardless of targets nearby; holds one Polyglot for manual usage", 0, 0, ActionTargets.Hostile, 80)
            .AddOption(PolyglotStrategy.XenoHold2, "XenoHold2", "Use Xenoglossy as optimal spender, regardless of targets nearby; holds two Polyglots for manual usage", 0, 0, ActionTargets.Hostile, 80)
            .AddOption(PolyglotStrategy.XenoHold3, "XenoHold3", "Use Xenoglossy as optimal spender; Holds all Polyglots for as long as possible", 0, 0, ActionTargets.Hostile, 80)
            .AddOption(PolyglotStrategy.FoulSpendAll, "FoulSpendAll", "Use Foul as optimal spender, regardless of targets nearby", 0, 0, ActionTargets.Hostile, 70)
            .AddOption(PolyglotStrategy.FoulHold1, "FoulHold1", "Use Foul as optimal spender, regardless of targets nearby; holds one Polyglot for manual usage", 0, 0, ActionTargets.Hostile, 70)
            .AddOption(PolyglotStrategy.FoulHold2, "FoulHold2", "Use Foul as optimal spender, regardless of targets nearby; holds two Polyglots for manual usage", 0, 0, ActionTargets.Hostile, 70)
            .AddOption(PolyglotStrategy.FoulHold3, "FoulHold3", "Use Foul as optimal spender; Holds all Polyglots for as long as possible", 0, 0, ActionTargets.Hostile, 70)
            .AddOption(PolyglotStrategy.ForceXeno, "Force Xenoglossy", "Force use of Xenoglossy", 0, 0, ActionTargets.Hostile, 80)
            .AddOption(PolyglotStrategy.ForceFoul, "Force Foul", "Force use of Foul", 0, 0, ActionTargets.Hostile, 70)
            .AddOption(PolyglotStrategy.Delay, "Delay", "Delay the use of Polyglot abilities for manual or strategic usage", 0, 0, ActionTargets.Hostile, 70)
            .AddAssociatedActions(AID.Xenoglossy, AID.Foul);
        res.Define(Track.Manafont).As<ManafontStrategy>("Manafont", "M.font", uiPriority: 165)
            .AddOption(ManafontStrategy.Automatic, "Auto", "Automatically decide when to use Manafont", 0, 0, ActionTargets.Self, 30)
            .AddOption(ManafontStrategy.Force, "Force", "Force the use of Manafont (180s TotalCD), regardless of weaving conditions", 180, 0, ActionTargets.Self, 30, 83)
            .AddOption(ManafontStrategy.ForceWeave, "ForceWeave", "Force the use of Manafont (180s TotalCD) in any next possible weave slot", 180, 0, ActionTargets.Self, 30, 83)
            .AddOption(ManafontStrategy.ForceEX, "ForceEX", "Force the use of Manafont (100s TotalCD), regardless of weaving conditions", 100, 0, ActionTargets.Self, 84)
            .AddOption(ManafontStrategy.ForceWeaveEX, "ForceWeaveEX", "Force the use of Manafont (100s TotalCD) in any next possible weave slot", 100, 0, ActionTargets.Self, 84)
            .AddOption(ManafontStrategy.Delay, "Delay", "Delay the use of Manafont for strategic reasons", 0, 0, ActionTargets.Self, 30)
            .AddAssociatedActions(AID.Manafont);
        res.Define(Track.Triplecast).As<TriplecastStrategy>("T.cast", uiPriority: 170)
            .AddOption(TriplecastStrategy.Automatic, "Auto", "Use any charges available during Ley Lines window or every 2 minutes (NOTE: does not take into account charge overcap, will wait for 2 minute windows to spend both)", 0, 0, ActionTargets.Self, 66)
            .AddOption(TriplecastStrategy.Force, "Force", "Force the use of Triplecast; uses all charges", 60, 15, ActionTargets.Self, 66)
            .AddOption(TriplecastStrategy.Force1, "Force1", "Force the use of Triplecast; holds one charge for manual usage", 60, 15, ActionTargets.Self, 66)
            .AddOption(TriplecastStrategy.ForceWeave, "ForceWeave", "Force the use of Triplecast in any next possible weave slot", 60, 15, ActionTargets.Self, 66)
            .AddOption(TriplecastStrategy.ForceWeave1, "ForceWeave1", "Force the use of Triplecast in any next possible weave slot; holds one charge for manual usage", 60, 15, ActionTargets.Self, 66)
            .AddOption(TriplecastStrategy.Delay, "Delay", "Delay the use of Triplecast", 0, 0, ActionTargets.Self, 66)
            .AddAssociatedActions(AID.Triplecast);
        res.Define(Track.LeyLines).As<LeyLinesStrategy>("L.Lines", uiPriority: 170)
            .AddOption(LeyLinesStrategy.Automatic, "Auto", "Automatically decide when to use Ley Lines", 0, 0, ActionTargets.Self, 2)
            .AddOption(LeyLinesStrategy.Force, "Force", "Force the use of Ley Lines, regardless of weaving conditions", 120, 30, ActionTargets.Self, 2)
            .AddOption(LeyLinesStrategy.Force1, "Force1", "Force the use of Ley Lines; holds one charge for manual usage", 120, 30, ActionTargets.Self, 2)
            .AddOption(LeyLinesStrategy.ForceWeave, "ForceWeave", "Force the use of Ley Lines in any next possible weave slot", 120, 30, ActionTargets.Self, 2)
            .AddOption(LeyLinesStrategy.ForceWeave1, "ForceWeave1", "Force the use of Ley Lines in any next possible weave slot; holds one charge for manual usage", 120, 30, ActionTargets.Self, 2)
            .AddOption(LeyLinesStrategy.Delay, "Delay", "Delay the use of Ley Lines", 0, 0, ActionTargets.Self, 2)
            .AddAssociatedActions(AID.LeyLines);
        res.Define(Track.Potion).As<PotionStrategy>("Potion", uiPriority: 160)
            .AddOption(PotionStrategy.Manual, "Manual", "Do not use automatically")
            .AddOption(PotionStrategy.AlignWithRaidBuffs, "AlignWithRaidBuffs", "Align with buffs (to ensure use on 2-minute windows)", 270, 30, ActionTargets.Self)
            .AddOption(PotionStrategy.Immediate, "Immediate", "Use ASAP, regardless of any buffs", 270, 30, ActionTargets.Self)
            .AddAssociatedAction(ActionDefinitions.IDPotionInt);
        res.Define(Track.TPUS).As<TPUSStrategy>("Transpose & Umbral Soul", "TP/US", uiPriority: 160)
            .AddOption(TPUSStrategy.Allow, "Allow", "Allow Transpose & Umbral Soul combo when out of combat or no targetable enemy is nearby", 0, 0, ActionTargets.Self, 35)
            .AddOption(TPUSStrategy.OOConly, "OOConly", "Only use Transpose & Umbral Soul combo when fully out of combat", 0, 0, ActionTargets.Self, 35)
            .AddOption(TPUSStrategy.Forbid, "Forbid", "Forbid Transpose & Umbral Soul combo", 0, 0, ActionTargets.Self, 35)
            .AddAssociatedActions(AID.Transpose, AID.UmbralSoul);
        res.Define(Track.Casting).As<CastingOption>("Casting", uiPriority: 155)
            .AddOption(CastingOption.Allow, "Allow", "Allow casting while moving")
            .AddOption(CastingOption.Forbid, "Forbid", "Forbid casting while moving");
        res.DefineOGCD(Track.Transpose, AID.Transpose, "Transpose", "Transpose", uiPriority: 125, 5, 0, ActionTargets.Self, 4);
        res.DefineOGCD(Track.Amplifier, AID.Amplifier, "Amplifier", "Amplifier", uiPriority: 170, 120, 0, ActionTargets.Self, 86);
        res.DefineOGCD(Track.Retrace, AID.Retrace, "Retrace", "Retrace", uiPriority: 170, 40, 0, ActionTargets.Self, 96);
        res.DefineOGCD(Track.BTL, AID.BetweenTheLines, "BTL", "Between the Lines", uiPriority: 170, 40, 0, ActionTargets.Self, 62);

        return res;
    }
    #endregion

    #region Priorities
    public enum GCDPriority //priorities for GCDs (higher number = higher priority)
    {
        None = 0,

        //Rotation
        SixthStep = 100,
        FifthStep = 125,
        FourthStep = 150,
        ThirdStep = 175,
        SecondStep = 200,
        FirstStep = 250,
        ForcedStep = 299,

        //GCDs
        Standard = 300,       //standard abilities
        DOT = 350,            //damage-over-time abilities
        FlareStar = 375,      //Flare Star
        Despair = 400,        //Despair
        F3P = 450,            //Fire III proc
        NeedB3 = 460,         //Need to use Blizzard III
        Polyglot = 475,       //Polyglots
        Paradox = 500,        //Paradox

        //Necessities
        NeedDOT = 600,        //Need to apply DOTs
        NeedF3P = 625,        //Need to use Fire III proc
        NeedPolyglot = 650,   //Need to use Polyglots

        //Moving
        Moving3 = 700,        //Moving (3rd priority)
        Moving2 = 710,        //Moving (2nd priority)
        Moving1 = 720,        //Moving (1st priority)

        //Forced
        ForcedGCD = 900,      //Forced GCDs

        //Opener
        Opener = 1000,        //Opener
    }
    public enum OGCDPriority //priorities for oGCDs (higher number = higher priority)
    {
        None = 0,
        Transpose = 400,      //Transpose
        Manafont = 450,       //Manafont
        LeyLines = 500,       //Ley Lines
        Amplifier = 550,      //Amplifier
        Triplecast = 600,     //Triplecast
        Potion = 800,         //Potion
        ForcedOGCD = 900,     //Forced oGCDs
    }
    #endregion

    #region Upgrade Paths
    private AID BestThunderST
        => Unlocked(AID.HighThunder) ? AID.HighThunder
        : Unlocked(AID.Thunder3) ? AID.Thunder3
        : AID.Thunder1;
    private AID BestThunderAOE
        => Unlocked(AID.HighThunder2) ? AID.HighThunder2
        : Unlocked(AID.Thunder4) ? AID.Thunder4
        : Unlocked(AID.Thunder2) ? AID.Thunder2
        : AID.Thunder1;
    private AID BestThunder
        => ShouldUseAOE ? BestThunderAOE : BestThunderST;
    private AID BestPolyglot
        => ShouldUseAOE ? AID.Foul : BestXenoglossy;
    private AID BestXenoglossy
        => Unlocked(AID.Xenoglossy) ? AID.Xenoglossy : AID.Foul;
    #endregion

    #region Module Variables
    private bool NoStance; //No stance
    private bool InAstralFire; //In Astral Fire
    private bool InUmbralIce; //In Umbral Ice
    private sbyte ElementStance; //Elemental Stance
    private byte Polyglots; //Polyglot Stacks
    private int MaxPolyglots; //Max Polyglot Stacks
    private byte UmbralHearts; //Umbral Hearts
    private int MaxUmbralHearts; //Max Umbral Hearts
    private int UmbralStacks; //Umbral Ice Stacks
    private int AstralStacks; //Astral Fire Stacks
    private int AstralSoulStacks; //Stacks for Flare Star (Lv100)
    private int EnochianTimer; //Enochian timer
    private float ElementTimer; //Time remaining on Enochian
    private bool ParadoxActive; //Paradox is active
    private bool canFoul; //Can use Foul
    private bool canXeno; //Can use Xenoglossy
    private bool canParadox; //Can use Paradox
    private bool canLL; //Can use Ley Lines
    private bool canAmp; //Can use Amplifier
    private bool canTC; //Can use Triplecast
    private bool canMF; //Can use Manafont
    private bool canRetrace; //Can use Retrace
    private bool canBTL; //Can use Between the Lines
    private bool hasThunderhead; //Has Thunderhead buff
    private float ThunderLeft; //Time left on DOT effect (30s base)
    public bool canOpen; //Can use opener
    private bool ShouldUseAOE;
    private int NumSplashTargets;
    private Enemy? BestSplashTargets;
    private Enemy? BestSplashTarget;
    #endregion

    #region Module Helpers
    public float GetCastTime(AID aid)
    {
        var aspect = ActionDefinitions.Instance.Spell(aid)!.Aspect;
        var castTime = EffectiveCastTime(aid);
        if (PlayerHasEffect(SID.Triplecast, 15f) || PlayerHasEffect(SID.Swiftcast, 10f))
            return 0f;
        if (aid == AID.Fire3 && PlayerHasEffect(SID.Firestarter, 30f)
            || aid == AID.Foul && Unlocked(TraitID.EnhancedFoul)
            || aspect == ActionAspect.Thunder && PlayerHasEffect(SID.Thunderhead, 30f)
            || aid == AID.Despair && Unlocked(TraitID.EnhancedAstralFire))
            return 0;
        if (castTime == 0)
            return 0;
        if (ElementStance == -3 && aspect == ActionAspect.Fire ||
            ElementStance == 3 && aspect == ActionAspect.Ice)
            castTime *= 0.5f;
        return castTime;
    }
    #endregion

    public override void Execution(StrategyValues strategy, Enemy? primaryTarget)
    {
        #region Variables
        var gauge = World.Client.GetGauge<BlackMageGauge>(); //Retrieve BLM gauge
        NoStance = ElementStance is 0 and not (1 or 2 or 3 or -1 or -2 or -3); //No stance
        ElementStance = gauge.ElementStance; //Elemental Stance
        InAstralFire = ElementStance is 1 or 2 or 3 and not (0 or -1 or -2 or -3); //In Astral Fire
        InUmbralIce = ElementStance is -1 or -2 or -3 and not (0 or 1 or 2 or 3); //In Umbral Ice
        Polyglots = gauge.PolyglotStacks; //Polyglot Stacks
        UmbralHearts = gauge.UmbralHearts; //Umbral Hearts
        MaxUmbralHearts = Unlocked(TraitID.UmbralHeart) ? 3 : 0;
        UmbralStacks = gauge.UmbralStacks; //Umbral Ice Stacks
        AstralStacks = gauge.AstralStacks; //Astral Fire Stacks
        AstralSoulStacks = gauge.AstralSoulStacks; //Stacks for Flare Star (Lv100)
        ParadoxActive = gauge.ParadoxActive; //Paradox is active
        EnochianTimer = gauge.EnochianTimer; //Enochian timer
        ElementTimer = gauge.ElementTimeRemaining / 1000f; //Time remaining on current element
        MaxPolyglots = Unlocked(TraitID.EnhancedPolyglotII) ? 3 : Unlocked(TraitID.EnhancedPolyglot) ? 2 : 1;
        canFoul = Unlocked(AID.Foul) && Polyglots > 0; //Can use Foul
        canXeno = Unlocked(AID.Xenoglossy) && Polyglots > 0; //Can use Xenoglossy
        canParadox = Unlocked(AID.Paradox) && ParadoxActive; //Can use Paradox
        canLL = Unlocked(AID.LeyLines) && TotalCD(AID.LeyLines) <= 120 && SelfStatusLeft(SID.LeyLines, 30) == 0; //Can use Ley Lines
        canAmp = ActionReady(AID.Amplifier); //Can use Amplifier
        canTC = Unlocked(AID.Triplecast) && TotalCD(AID.Triplecast) <= 60 && SelfStatusLeft(SID.Triplecast) == 0; //Can use Triplecast
        canMF = ActionReady(AID.Manafont); //Can use Manafont
        canRetrace = ActionReady(AID.Retrace) && PlayerHasEffect(SID.LeyLines, 30); //Can use Retrace
        canBTL = ActionReady(AID.BetweenTheLines) && PlayerHasEffect(SID.LeyLines, 30); //Can use Between the Lines
        hasThunderhead = PlayerHasEffect(SID.Thunderhead, 30); //Has Thunderhead buff
        ThunderLeft = Utils.MaxAll( //Time left on DOT effect
            StatusDetails(BestSplashTarget?.Actor, SID.Thunder, Player.InstanceID, 24).Left,
            StatusDetails(BestSplashTarget?.Actor, SID.ThunderII, Player.InstanceID, 18).Left,
            StatusDetails(BestSplashTarget?.Actor, SID.ThunderIII, Player.InstanceID, 27).Left,
            StatusDetails(BestSplashTarget?.Actor, SID.ThunderIV, Player.InstanceID, 21).Left,
            StatusDetails(BestSplashTarget?.Actor, SID.HighThunder, Player.InstanceID, 30).Left,
            StatusDetails(BestSplashTarget?.Actor, SID.HighThunderII, Player.InstanceID, 24).Left);
        ShouldUseAOE = Unlocked(AID.Blizzard2) && NumSplashTargets > 2;
        (BestSplashTargets, NumSplashTargets) = GetBestTarget(BestSplashTarget, 25, IsSplashTarget);
        BestSplashTarget = ShouldUseAOE ? BestSplashTargets : primaryTarget;
        canOpen = TotalCD(AID.LeyLines) <= 120
                && TotalCD(AID.Triplecast) <= 0.1f
                && TotalCD(AID.Manafont) <= 0.1f
                && TotalCD(AID.Amplifier) <= 0.1f;

        #region Strategy Definitions
        var AOE = strategy.Option(Track.AOE); //AOE track
        var AOEStrategy = AOE.As<AOEStrategy>(); //AOE strategy
        var movementStrat = strategy.Option(Track.Movement).As<MovementStrategy>();
        var thunder = strategy.Option(Track.Thunder); //Thunder track
        var thunderStrat = thunder.As<ThunderStrategy>(); //Thunder strategy
        var polyglot = strategy.Option(Track.Polyglot); //Polyglot track
        var polyglotStrat = polyglot.As<PolyglotStrategy>(); //Polyglot strategy
        var mf = strategy.Option(Track.Manafont); //Manafont track
        var mfStrat = mf.As<ManafontStrategy>(); //Manafont strategy
        var tc = strategy.Option(Track.Triplecast); //Triplecast track
        var tcStrat = tc.As<TriplecastStrategy>(); //Triplecast strategy
        var ll = strategy.Option(Track.LeyLines); //Ley Lines track
        var llStrat = ll.As<LeyLinesStrategy>(); //Ley Lines strategy
        var amp = strategy.Option(Track.Amplifier); //Amplifier track
        var ampStrat = amp.As<OffensiveStrategy>(); //Amplifier strategy
        var retrace = strategy.Option(Track.Retrace); //Retrace track
        var retraceStrat = retrace.As<OffensiveStrategy>(); //Retrace strategy
        var btl = strategy.Option(Track.BTL); //Between the Lines track
        var btlStrat = btl.As<OffensiveStrategy>(); //Between the Lines strategy
        var potionStrat = strategy.Option(Track.Potion).As<PotionStrategy>(); //Potion strategy
        var tpusStrat = strategy.Option(Track.TPUS).As<TPUSStrategy>(); //Transpose/Umbral Soul strategy
        var movingOption = strategy.Option(Track.Casting).As<CastingOption>(); //Casting while moving strategy
        var forceST = AOEStrategy is AOEStrategy.ForceST; //Force single target
        var forceAOE = AOEStrategy is AOEStrategy.ForceAOE; //Force AOE
        #endregion

        #endregion

        #region Full Rotation Execution

        #region Movement
        if (Player.InCombat &&
            BestSplashTarget?.Actor != null &&
            IsMoving)
        {
            if (movementStrat is MovementStrategy.Allow
                or MovementStrategy.AllowNoScathe
                or MovementStrategy.OnlyGCDs)
            {
                // GCDs
                if (!PlayerHasEffect(SID.Swiftcast, 10) || !PlayerHasEffect(SID.Triplecast, 15))
                {
                    if (Unlocked(TraitID.EnhancedPolyglot) && Polyglots > 0)
                        QueueGCD(forceST ? BestXenoglossy : forceAOE ? AID.Foul : BestPolyglot,
                                 TargetChoice(polyglot) ?? (forceST ? primaryTarget?.Actor : BestSplashTarget?.Actor),
                                 GCDPriority.Moving1);

                    if (PlayerHasEffect(SID.Firestarter, 30))
                        QueueGCD(AID.Fire3,
                                 TargetChoice(AOE) ?? primaryTarget?.Actor,
                                 GCDPriority.Moving1);

                    if (hasThunderhead)
                        QueueGCD(forceST ? BestThunderST : forceAOE ? BestThunderAOE : BestThunder,
                                 TargetChoice(thunder) ?? (forceST ? primaryTarget?.Actor : BestSplashTarget?.Actor),
                                 GCDPriority.Moving1);
                }
            }
            if (movementStrat is MovementStrategy.Allow
                or MovementStrategy.AllowNoScathe
                or MovementStrategy.OnlyOGCDs)
            {
                //OGCDs
                if (ActionReady(AID.Swiftcast) &&
                    !PlayerHasEffect(SID.Triplecast, 15))
                    QueueOGCD(AID.Swiftcast, Player, GCDPriority.Moving2);
                if (canTC &&
                    !PlayerHasEffect(SID.Swiftcast, 10))
                    QueueOGCD(AID.Triplecast, Player, GCDPriority.Moving3);
            }
            if (movementStrat is MovementStrategy.Allow
                or MovementStrategy.OnlyScathe)
            {
                if (Unlocked(AID.Scathe) && MP >= 800)
                    QueueGCD(AID.Scathe, TargetChoice(AOE) ?? primaryTarget?.Actor, GCDPriority.Moving1);
            }
        }
        #endregion

        #region Out of Combat
        if (BestSplashTarget?.Actor == null &&
            (tpusStrat == TPUSStrategy.Allow && (!Player.InCombat || Player.InCombat && Hints.NumPriorityTargetsInAOECircle(Player.Position, 30) == 0) ||
            tpusStrat == TPUSStrategy.OOConly && !Player.InCombat))
        {
            if (Unlocked(AID.Transpose))
            {
                if (!Unlocked(AID.UmbralSoul))
                {
                    if (TotalCD(AID.Transpose) < 0.6f &&
                        (InAstralFire || InUmbralIce))
                        QueueOGCD(AID.Transpose, Player, OGCDPriority.Transpose);
                }
                if (Unlocked(AID.UmbralSoul))
                {
                    if (InAstralFire)
                        QueueOGCD(AID.Transpose, Player, OGCDPriority.Transpose);
                    if (InUmbralIce &&
                        (ElementTimer <= 14 || UmbralStacks < 3 || UmbralHearts != MaxUmbralHearts))
                        QueueGCD(AID.UmbralSoul, Player, GCDPriority.Standard);
                }
            }
        }
        #endregion

        #region Standard Rotations
        if (movingOption is CastingOption.Allow ||
            movingOption is CastingOption.Forbid &&
            (!IsMoving || //if not moving
            PlayerHasEffect(SID.Swiftcast, 10) || //or has Swiftcast
            PlayerHasEffect(SID.Triplecast, 15) || //or has Triplecast
            canParadox && ElementTimer < SpSGCDLength * 3 && MP >= 1600 || LastActionUsed(AID.Blizzard4) || //or can use Paradox
            SelfStatusLeft(SID.Firestarter, 30) is < 25 and not 0 || //or can use F3P
            Unlocked(TraitID.EnhancedAstralFire) && MP is < 1600 and not 0)) //instant cast Despair 
        {
            if (AOEStrategy is AOEStrategy.Auto)
                BestRotation(TargetChoice(AOE) ?? BestSplashTarget?.Actor);
            if (forceST)
                BestST(TargetChoice(AOE) ?? primaryTarget?.Actor);
            if (forceAOE)
                BestAOE(TargetChoice(AOE) ?? BestSplashTarget?.Actor);
        }
        #endregion

        #region Cooldowns
        //Thunder
        if (ShouldUseThunder(BestSplashTarget?.Actor, thunderStrat)) //if Thunder should be used based on strategy
        {
            if (AOEStrategy is AOEStrategy.Auto)
                QueueGCD(BestThunder,
                    TargetChoice(thunder),
                    ThunderLeft <= 3 ? GCDPriority.NeedDOT :
                    GCDPriority.DOT);
            if (forceST)
                QueueGCD(BestThunderST,
                    TargetChoice(thunder) ?? BestSplashTarget?.Actor,
                    ThunderLeft <= 3 ? GCDPriority.NeedDOT :
                    GCDPriority.DOT);
            if (forceAOE)
                QueueGCD(BestThunderAOE,
                    TargetChoice(thunder) ?? BestSplashTarget?.Actor,
                    ThunderLeft <= 3 ? GCDPriority.NeedDOT :
                    GCDPriority.DOT);
        }
        //Polyglots
        if (ShouldUsePolyglot(BestSplashTarget?.Actor, polyglotStrat)) //if Polyglot should be used based on strategy
        {
            if (polyglotStrat is PolyglotStrategy.AutoSpendAll
                or PolyglotStrategy.AutoHold1
                or PolyglotStrategy.AutoHold2
                or PolyglotStrategy.AutoHold3)
                QueueGCD(BestPolyglot,
                    TargetChoice(polyglot) ?? BestSplashTarget?.Actor,
                    polyglotStrat is PolyglotStrategy.ForceXeno ? GCDPriority.ForcedGCD
                    : Polyglots == MaxPolyglots && EnochianTimer <= 5000 ? GCDPriority.NeedPolyglot
                    : GCDPriority.Polyglot);
            if (polyglotStrat is PolyglotStrategy.XenoSpendAll
                or PolyglotStrategy.XenoHold1
                or PolyglotStrategy.XenoHold2
                or PolyglotStrategy.XenoHold3)
                QueueGCD(BestXenoglossy,
                    TargetChoice(polyglot) ?? BestSplashTarget?.Actor,
                    polyglotStrat is PolyglotStrategy.ForceXeno ? GCDPriority.ForcedGCD
                    : Polyglots == MaxPolyglots && EnochianTimer <= 5000 ? GCDPriority.NeedPolyglot
                    : GCDPriority.Polyglot);
            if (polyglotStrat is PolyglotStrategy.FoulSpendAll
                or PolyglotStrategy.FoulHold1
                or PolyglotStrategy.FoulHold2
                or PolyglotStrategy.FoulHold3)
                QueueGCD(AID.Foul,
                    TargetChoice(polyglot) ?? BestSplashTarget?.Actor,
                    polyglotStrat is PolyglotStrategy.ForceFoul ? GCDPriority.ForcedGCD
                    : Polyglots == MaxPolyglots && EnochianTimer <= 5000 ? GCDPriority.NeedPolyglot
                    : GCDPriority.Polyglot);
        }
        //LeyLines
        if (ShouldUseLeyLines(BestSplashTarget?.Actor, llStrat))
            QueueOGCD(AID.LeyLines,
                Player,
                llStrat is LeyLinesStrategy.Force
                or LeyLinesStrategy.Force1
                or LeyLinesStrategy.ForceWeave1
                or LeyLinesStrategy.ForceWeave1
                ? OGCDPriority.ForcedOGCD
                : OGCDPriority.LeyLines);
        //Triplecast
        if (ShouldUseTriplecast(BestSplashTarget?.Actor, tcStrat))
            QueueOGCD(AID.Triplecast,
                Player,
                tcStrat is TriplecastStrategy.Force
                or TriplecastStrategy.Force1
                or TriplecastStrategy.ForceWeave
                or TriplecastStrategy.ForceWeave1
                ? OGCDPriority.ForcedOGCD
                : OGCDPriority.Triplecast);
        //Amplifier
        if (ShouldUseAmplifier(BestSplashTarget?.Actor, ampStrat))
            QueueOGCD(AID.Amplifier,
                Player,
                ampStrat is OffensiveStrategy.Force
                or OffensiveStrategy.AnyWeave
                or OffensiveStrategy.EarlyWeave
                or OffensiveStrategy.LateWeave
                ? OGCDPriority.ForcedOGCD
                : OGCDPriority.Amplifier);
        //Manafont
        if (ShouldUseManafont(BestSplashTarget?.Actor, mfStrat))
            QueueOGCD(AID.Manafont,
                Player,
                mfStrat is ManafontStrategy.Force
                or ManafontStrategy.ForceWeave
                or ManafontStrategy.ForceEX
                or ManafontStrategy.ForceWeaveEX
                ? OGCDPriority.ForcedOGCD
                : OGCDPriority.Manafont);
        //Retrace
        //TODO: more options?
        if (ShouldUseRetrace(retraceStrat))
            QueueOGCD(AID.Retrace,
                Player,
                OGCDPriority.ForcedOGCD);
        //Between the Lines
        //TODO: Utility maybe?
        if (ShouldUseBTL(btlStrat))
            QueueOGCD(AID.BetweenTheLines,
                Player,
                OGCDPriority.ForcedOGCD);
        //Potion
        if (potionStrat is PotionStrategy.AlignWithRaidBuffs && TotalCD(AID.LeyLines) < 5 ||
            potionStrat is PotionStrategy.Immediate)
            Hints.ActionsToExecute.Push(ActionDefinitions.IDPotionInt, Player, ActionQueue.Priority.VeryHigh + (int)OGCDPriority.Potion, 0, GCD - 0.9f);
        #endregion

        #endregion

        #region AI
        GoalZoneSingle(25);
        #endregion
    }

    #region Rotation Helpers
    private void BestRotation(Actor? target)
    {
        if (ShouldUseAOE)
            BestAOE(target);
        if (!ShouldUseAOE)
            BestST(target);
    }
    private void BestST(Actor? target) //Single-target rotation based on level
    {
        if (In25y(target))
        {
            if (NoStance) //if no stance is active
            {
                if (Unlocked(AID.Blizzard3) &&
                    MP < 9600 &&
                    Player.InCombat) //if Blizzard III is unlocked
                    QueueGCD(AID.Blizzard3, target, GCDPriority.NeedB3); //Queue Blizzard III
                if (Unlocked(AID.Fire3) &&
                    (TotalCD(AID.Manafont) < 5 && TotalCD(AID.LeyLines) <= 121 && MP >= 10000 || !Player.InCombat && World.Client.CountdownRemaining <= 4)) //F3 opener
                    QueueGCD(AID.Fire3, target, canOpen ? GCDPriority.Opener : GCDPriority.NeedB3);
            }
            if (Player.Level is >= 1 and <= 34)
            {
                //Fire
                if (Unlocked(AID.Fire1) && //if Fire is unlocked
                    NoStance && MP >= 800 || //if no stance is active and MP is 800 or more
                    InAstralFire && MP >= 1600) //or if Astral Fire is active and MP is 1600 or more
                    QueueGCD(AID.Fire1, target, GCDPriority.Standard); //Queue Fire
                //Ice
                //TODO: Fix Blizzard I still casting once after at 10000MP due to MP tick not counting fast enough before next cast
                if (InUmbralIce && MP < 9500) //if Umbral Ice is active and MP is not max
                    QueueGCD(AID.Blizzard1, target, GCDPriority.Standard); //Queue Blizzard
                //Transpose 
                if (ActionReady(AID.Transpose) && //if Transpose is unlocked & off cooldown
                    InAstralFire && MP < 1600 || //if Astral Fire is active and MP is less than 1600
                    InUmbralIce && MP == 10000) //or if Umbral Ice is active and MP is max
                    QueueOGCD(AID.Transpose, Player, OGCDPriority.Transpose); //Queue Transpose

            }
            if (Player.Level is >= 35 and <= 59)
            {
                if (InUmbralIce) //if Umbral Ice is active
                {
                    //Step 1 - max stacks in UI
                    if (LastActionUsed(AID.Blizzard3)) //if Blizzard III was just used
                    {
                        if (!Unlocked(AID.Blizzard4) && UmbralStacks == 3) //if Blizzard IV is not unlocked and Umbral Ice stacks are max
                            QueueGCD(AID.Blizzard1, target, GCDPriority.FirstStep); //Queue Blizzard I
                        if (Unlocked(AID.Blizzard4) && UmbralHearts != MaxUmbralHearts) //if Blizzard IV is unlocked and Umbral Hearts are not max
                            QueueGCD(AID.Blizzard4, target, GCDPriority.FirstStep); //Queue Blizzard IV
                    }
                    //Step 2 - swap from UI to AF
                    if (Unlocked(AID.Fire3) && //if Fire III is unlocked
                        LastActionUsed(AID.Blizzard1) && //and Blizzard I was just used
                        MP < 10000 && //and MP is less than max
                        Unlocked(TraitID.UmbralHeart) ? UmbralHearts == MaxUmbralHearts : UmbralHearts == 0) //and Umbral Hearts are max if unlocked, or 0 if not
                        QueueGCD(AID.Fire3, target, LastActionUsed(AID.Blizzard1) ? GCDPriority.ForcedStep : GCDPriority.SecondStep); //Queue Fire III, increase priority if Blizzard I was just used
                }
                if (InAstralFire) //if Astral Fire is active
                {
                    //Step 1 - Fire 1
                    if (MP >= 1600) //if MP is 1600 or more
                        QueueGCD(AID.Fire1, target, GCDPriority.FirstStep); //Queue Fire I
                    //Step 2B - F3P 
                    if (SelfStatusLeft(SID.Firestarter, 30) is < 25 and not 0 && //if Firestarter buff is active and not 0
                        AstralStacks == 3) //and Umbral Hearts are 0
                        QueueGCD(AID.Fire3, target, GCDPriority.ForcedStep); //Queue Fire III (AF3 F3P)
                    //Step 3 - swap from AF to UI
                    if (Unlocked(AID.Blizzard3) && //if Blizzard III is unlocked
                        MP < 1600) //and MP is less than 400
                        QueueGCD(AID.Blizzard3, target, GCDPriority.SecondStep); //Queue Blizzard III
                }
            }
            if (Player.Level is >= 60 and <= 71)
            {
                if (InUmbralIce) //if Umbral Ice is active
                {
                    //Step 1 - max stacks in UI
                    if (Unlocked(AID.Blizzard4) && //if Blizzard IV is unlocked
                        LastActionUsed(AID.Blizzard3) || UmbralHearts != MaxUmbralHearts) //and Blizzard III was just used or Umbral Hearts are not max
                        QueueGCD(AID.Blizzard4, target, GCDPriority.FirstStep); //Queue Blizzard IV
                    //Step 2 - swap from UI to AF
                    if (Unlocked(AID.Fire3) && //if Fire III is unlocked
                        UmbralHearts == MaxUmbralHearts) //and Umbral Hearts are max
                        QueueGCD(AID.Fire3, target, GCDPriority.SecondStep); //Queue Fire III
                }
                if (InAstralFire) //if Astral Fire is active
                {
                    //Step 1-3-7 - Fire IV
                    if (MP >= 1600) //and MP is 1600 or more
                        QueueGCD(AID.Fire4, target, GCDPriority.FirstStep); //Queue Fire IV
                    //Step 4A - Fire 1
                    if (ElementTimer <= SpSGCDLength * 3 && //if time remaining on current element is less than 3x GCDs
                        MP >= 4000) //and MP is 4000 or more
                        QueueGCD(AID.Fire1, target, ElementTimer <= 5 && MP >= 4000 ? GCDPriority.Paradox : GCDPriority.SecondStep); //Queue Fire I, increase priority if less than 3s left on element
                    //Step 4B - F3P 
                    if (SelfStatusLeft(SID.Firestarter, 30) is < 25 and not 0 && //if Firestarter buff is active and not 0
                        AstralStacks == 3) //and Umbral Hearts are 0
                        QueueGCD(AID.Fire3, target, GCDPriority.ForcedStep); //Queue Fire III (AF3 F3P)
                    //Step 8 - swap from AF to UI
                    if (Unlocked(AID.Blizzard3) && //if Blizzard III is unlocked
                        MP < 1600) //and MP is less than 400
                        QueueGCD(AID.Blizzard3, target, GCDPriority.ThirdStep); //Queue Blizzard III
                }
            }
            if (Player.Level is >= 72 and <= 89)
            {
                if (InUmbralIce) //if Umbral Ice is active
                {
                    //Step 1 - max stacks in UI
                    if (Unlocked(AID.Blizzard4) && //if Blizzard IV is unlocked
                        LastActionUsed(AID.Blizzard3) || UmbralHearts != MaxUmbralHearts) //and Blizzard III was just used or Umbral Hearts are not max
                        QueueGCD(AID.Blizzard4, target, GCDPriority.FirstStep); //Queue Blizzard IV
                    //Step 2 - swap from UI to AF
                    if (Unlocked(AID.Fire3) && //if Fire III is unlocked
                        UmbralHearts == MaxUmbralHearts) //and Umbral Hearts are max
                        QueueGCD(AID.Fire3, target, GCDPriority.SecondStep); //Queue Fire III
                }
                if (InAstralFire) //if Astral Fire is active
                {
                    //Step 1-3-7 - Fire IV
                    if (MP >= 1600) //and MP is 1600 or more
                        QueueGCD(AID.Fire4, target, GCDPriority.FirstStep); //Queue Fire IV
                    //Step 4A - Fire 1
                    if (ElementTimer <= GetCastTime(AID.Fire1) * 3 && //if time remaining on current element is less than 3x GCDs
                        MP >= 4000) //and MP is 4000 or more
                        QueueGCD(AID.Fire1, target, ElementTimer <= GetCastTime(AID.Fire1) * 3 && MP >= 4000 ? GCDPriority.Paradox : GCDPriority.SecondStep); //Queue Fire I, increase priority if less than 3s left on element
                    //Step 4B - F3P 
                    if (SelfStatusLeft(SID.Firestarter, 30) is < 25 and not 0 && //if Firestarter buff is active and not 0
                        AstralStacks == 3) //and Umbral Hearts are 0
                        QueueGCD(AID.Fire3, target, GCDPriority.ForcedStep); //Queue Fire III (AF3 F3P)
                    //Step 8 - Despair 
                    if (Unlocked(AID.Despair) && //if Despair is unlocked
                        (MP is < 1600 and >= 800 || //if MP is less than 1600 and not 0
                        MP is <= 4000 and >= 800 && ElementTimer <= GetCastTime(AID.Despair) * 2)) //or if we dont have enough time for last F4s
                        QueueGCD(AID.Despair, target, ElementTimer <= GetCastTime(AID.Despair) * 2 ? GCDPriority.ForcedGCD : GCDPriority.ThirdStep); //Queue Despair
                    //Step 9 - swap from AF to UI 
                    if (MP <= 400) //and MP is less than 400
                        QueueGCD(AID.Blizzard3, target, GCDPriority.FourthStep); //Queue Blizzard III
                }
            }
            if (Player.Level is >= 90 and <= 99)
            {
                if (InUmbralIce) //if Umbral Ice is active
                {
                    //Step 1 - max stacks in UI
                    if (Unlocked(AID.Blizzard4) && //if Blizzard IV is unlocked
                        LastActionUsed(AID.Blizzard3) || UmbralHearts != MaxUmbralHearts) //and Blizzard III was just used or Umbral Hearts are not max
                        QueueGCD(AID.Blizzard4, target, GCDPriority.FirstStep); //Queue Blizzard IV
                    //Step 2 - Ice Paradox
                    if (canParadox && //if Paradox is unlocked and Paradox is active
                        LastActionUsed(AID.Blizzard4)) //and Blizzard IV was just used
                        QueueGCD(AID.Paradox, target, GCDPriority.SecondStep); //Queue Paradox
                    //Step 3 - swap from UI to AF
                    if (Unlocked(AID.Fire3) && //if Fire III is unlocked
                        UmbralHearts == MaxUmbralHearts) //and Umbral Hearts are max
                        QueueGCD(AID.Fire3, target, GCDPriority.ThirdStep); //Queue Fire III
                }
                if (InAstralFire) //if Astral Fire is active
                {
                    //Step 1-4, 6 & 7 - Fire IV
                    if (MP >= 1600) //and MP is 1600 or more
                        QueueGCD(AID.Fire4, target, GCDPriority.FirstStep); //Queue Fire IV
                    //Step 5A - Paradox
                    if (canParadox && //if Paradox is unlocked and Paradox is active
                        ElementTimer < SpSGCDLength * 3 && //and time remaining on current element is less than 3x GCDs
                        MP >= 1600) //and MP is 1600 or more
                        QueueGCD(AID.Paradox, target, ElementTimer <= 3 ? GCDPriority.Paradox : GCDPriority.SecondStep); //Queue Paradox, increase priority if less than 3s left on element
                    //Step 4B - F3P 
                    if (SelfStatusLeft(SID.Firestarter, 30) is < 25 and not 0 && //if Firestarter buff is active and not 0
                        AstralStacks == 3) //and Umbral Hearts are 0
                        QueueGCD(AID.Fire3, target, GCDPriority.ForcedStep); //Queue Fire III (AF3 F3P)
                    //Step 8 - Despair 
                    if (Unlocked(AID.Despair) && //if Despair is unlocked
                        (MP is < 1600 and >= 800 || //if MP is less than 1600 and not 0
                        MP is <= 4000 and >= 800 && ElementTimer <= GetCastTime(AID.Despair) * 2)) //or if we dont have enough time for last F4s
                        QueueGCD(AID.Despair, target, ElementTimer <= GetCastTime(AID.Despair) * 2 ? GCDPriority.ForcedGCD : GCDPriority.ThirdStep); //Queue Despair
                    //Step 9 - swap from AF to UI
                    if (Unlocked(AID.Blizzard3) && //if Blizzard III is unlocked
                        MP <= 400) //and MP is less than 400
                        QueueGCD(AID.Blizzard3, target, GCDPriority.FourthStep); //Queue Blizzard III
                }
            }
            if (Player.Level is 100)
            {
                if (InUmbralIce) //if Umbral Ice is active
                {
                    //Step 1 - max stacks in UI
                    if (Unlocked(AID.Blizzard4) && //if Blizzard IV is unlocked
                        LastActionUsed(AID.Blizzard3) || UmbralHearts != MaxUmbralHearts) //and Blizzard III was just used or Umbral Hearts are not max
                        QueueGCD(AID.Blizzard4, target, GCDPriority.FirstStep); //Queue Blizzard IV
                    //Step 2 - Ice Paradox
                    if (canParadox && //if Paradox is unlocked and Paradox is active
                        LastActionUsed(AID.Blizzard4)) //and Blizzard IV was just used
                        QueueGCD(AID.Paradox, target, GCDPriority.SecondStep); //Queue Paradox
                    //Step 3 - swap from UI to AF
                    if (Unlocked(AID.Fire3) && //if Fire III is unlocked
                        UmbralHearts == MaxUmbralHearts) //and Umbral Hearts are max
                        QueueGCD(AID.Fire3, target, GCDPriority.ThirdStep); //Queue Fire III
                }
                if (InAstralFire) //if Astral Fire is active
                {
                    //Step 1-4, 6 & 7 - Fire IV
                    if (AstralSoulStacks != 6 && //and Astral Soul stacks are not max
                        MP >= 1600) //and MP is 1600 or more
                        QueueGCD(AID.Fire4, target, GCDPriority.FirstStep); //Queue Fire IV
                    //Step 5A - Paradox
                    if (ParadoxActive && //if Paradox is active
                        ElementTimer < SpSGCDLength * 3 && //and time remaining on current element is less than 3x GCDs
                        MP >= 1600) //and MP is 1600 or more
                        QueueGCD(AID.Paradox, target, ElementTimer <= 3 ? GCDPriority.Paradox : GCDPriority.SecondStep); //Queue Paradox, increase priority if less than 3s left on element
                    //Step 4B - F3P 
                    if (SelfStatusLeft(SID.Firestarter, 30) is < 25 and not 0 && //if Firestarter buff is active and not 0
                        AstralStacks == 3) //and Umbral Hearts are 0
                        QueueGCD(AID.Fire3, target, GCDPriority.ForcedStep); //Queue Fire III (AF3 F3P)
                    //Step 8 - Despair
                    if (Unlocked(AID.Despair) &&
                        (MP is < 1600 and not 0 || MP <= 1600 && ElementTimer <= 4)) //if MP is less than 1600 and not 0
                        QueueGCD(AID.Despair, target, MP <= 1600 && ElementTimer <= 4 ? GCDPriority.NeedPolyglot : GCDPriority.ThirdStep); //Queue Despair
                    //Step 9 - Flare Star
                    if (AstralSoulStacks == 6) //if Astral Soul stacks are max
                        QueueGCD(AID.FlareStar, target, GCDPriority.FourthStep); //Queue Flare Star
                    //Step 10A - skip Flare Star if we cant use it (cryge)
                    if (Unlocked(AID.Blizzard3) && //if Blizzard III is unlocked
                        MP <= 400 && //and MP is less than 400
                        AstralSoulStacks is < 6 and > 0) //and Astral Soul stacks are less than 6 but greater than 0
                        QueueGCD(AID.Blizzard3, target, GCDPriority.FifthStep); //Queue Blizzard III
                    //Step 10B - swap from AF to UI
                    if (Unlocked(AID.Blizzard3) && //if Blizzard III is unlocked
                        MP <= 400 && //and MP is less than 400
                        AstralSoulStacks == 0) //and Astral Soul stacks are 0
                        QueueGCD(AID.Blizzard3, target, GCDPriority.FifthStep); //Queue Blizzard III
                }
            }
        }
    }
    private void BestAOE(Actor? target) //AOE rotation based on level
    {
        if (In25y(target))
        {
            if (NoStance)
            {
                if (Unlocked(AID.Blizzard2) && !Unlocked(AID.HighBlizzard2))
                {
                    if (MP >= 10000)
                        QueueGCD(AID.Blizzard2, target, GCDPriority.NeedB3);
                    if (MP < 10000 && Player.InCombat)
                        QueueGCD(AID.Blizzard2, target, GCDPriority.NeedB3);
                }
                if (Unlocked(AID.HighBlizzard2))
                {
                    if (MP >= 10000)
                        QueueGCD(AID.HighBlizzard2, target, GCDPriority.NeedB3);
                    if (MP < 10000 && Player.InCombat)
                        QueueGCD(AID.HighBlizzard2, target, GCDPriority.NeedB3);
                }
            }
            if (Player.Level is >= 12 and <= 35)
            {
                //Fire
                if (Unlocked(AID.Fire2) && //if Fire is unlocked
                    InAstralFire && MP >= 3000) //or if Astral Fire is active and MP is 1600 or more
                    QueueGCD(AID.Fire2, target, GCDPriority.Standard); //Queue Fire II
                //Ice
                //TODO: MP tick is not fast enough before next cast, this will cause an extra unnecessary cast
                if (InUmbralIce &&
                    MP <= 9600)
                    QueueGCD(AID.Blizzard2, target, GCDPriority.Standard); //Queue Blizzard II
                //Transpose 
                if (ActionReady(AID.Transpose) && //if Transpose is unlocked & off cooldown
                    (InAstralFire && MP < 3000 || //if Astral Fire is active and MP is less than 1600
                    InUmbralIce && MP > 9600)) //or if Umbral Ice is active and MP is max
                    QueueOGCD(AID.Transpose, Player, OGCDPriority.Transpose); //Queue Transpose
                //if in AF but no F2 yet, TP back to UI for B2 spam
                if (InAstralFire && !Unlocked(AID.Fire2))
                    QueueOGCD(AID.Transpose, Player, OGCDPriority.Transpose);
            }
            if (Player.Level is >= 35 and <= 39)
            {
                if (InUmbralIce)
                {
                    //Step 1 - max stacks in UI
                    //TODO: MP tick is not fast enough before next cast, this will cause an extra unnecessary cast
                    if (Unlocked(AID.Blizzard2) &&
                        MP < 9600)
                        QueueGCD(AID.Blizzard2, target, GCDPriority.FirstStep);
                    //Step 2 - swap from UI to AF
                    if (Unlocked(AID.Fire2) &&
                        MP >= 9600 &&
                        UmbralStacks == 3)
                        QueueGCD(AID.Fire2, target, GCDPriority.SecondStep);
                }
                if (InAstralFire)
                {
                    if (MP >= 3000)
                        QueueGCD(AID.Fire2, target, GCDPriority.FirstStep);
                    if (Unlocked(AID.Blizzard2) &&
                        MP < 3000)
                        QueueGCD(AID.Blizzard2, target, GCDPriority.SecondStep);
                }
            }
            if (Player.Level is >= 40 and <= 49)
            {
                if (InUmbralIce)
                {
                    //Step 1 - max stacks in UI
                    if (Unlocked(AID.Blizzard2) &&
                        UmbralStacks < 3)
                        QueueGCD(AID.Blizzard2, target, GCDPriority.FirstStep);
                    //Step 2 - Freeze
                    if (Unlocked(AID.Freeze) && !LastActionUsed(AID.Freeze) &&
                        (LastActionUsed(AID.Blizzard2) || MP < 10000))
                        QueueGCD(AID.Freeze, target, GCDPriority.SecondStep);
                    //Step 3 - swap from UI to AF
                    if (Unlocked(AID.Fire2) &&
                        MP >= 10000 &&
                        UmbralStacks == 3)
                        QueueGCD(AID.Fire2, target, GCDPriority.ThirdStep);
                }
                if (InAstralFire)
                {
                    if (MP >= 3000)
                        QueueGCD(AID.Fire2, target, GCDPriority.FirstStep);
                    if (Unlocked(AID.Blizzard2) &&
                        MP < 3000)
                        QueueGCD(AID.Blizzard2, target, GCDPriority.SecondStep);
                }
            }
            if (Player.Level is >= 50 and <= 57)
            {
                if (InUmbralIce)
                {
                    //Step 1 - max stacks in UI
                    if (Unlocked(AID.Blizzard2) &&
                        UmbralStacks < 3)
                        QueueGCD(AID.Blizzard2, target, GCDPriority.FirstStep);
                    //Step 2 - Freeze
                    if (Unlocked(AID.Freeze) && !LastActionUsed(AID.Freeze) &&
                        (LastActionUsed(AID.Blizzard2) || MP < 10000))
                        QueueGCD(AID.Freeze, target, GCDPriority.SecondStep);
                    //Step 3 - swap from UI to AF
                    if (Unlocked(AID.Fire2) &&
                        MP >= 10000 &&
                        UmbralStacks == 3)
                        QueueGCD(AID.Fire2, target, GCDPriority.ThirdStep);
                }
                if (InAstralFire)
                {
                    //Step 1 - spam Fire 2
                    if (MP >= 3000)
                        QueueGCD(AID.Fire2, target, GCDPriority.FirstStep);
                    //Step 2 - Flare
                    if (Unlocked(AID.Flare) &&
                        MP < 3000)
                        QueueGCD(AID.Flare, target, GCDPriority.SecondStep);
                    //Step 3 - swap from AF to UI
                    if (Unlocked(AID.Blizzard2) &&
                        !Unlocked(AID.Flare) && MP < 3000 || //do your job quests, fool
                        Unlocked(AID.Flare) && MP < 400)
                        QueueGCD(AID.Blizzard2, target, MP < 400 ? GCDPriority.ForcedStep : GCDPriority.ThirdStep);
                }
            }
            if (Player.Level is >= 58 and <= 81)
            {
                if (InUmbralIce)
                {
                    //Step 1 - max stacks in UI
                    if (Unlocked(AID.Blizzard2) &&
                        UmbralStacks < 3)
                        QueueGCD(AID.Blizzard2, target, GCDPriority.FirstStep);
                    //Step 2 - Freeze
                    if (Unlocked(AID.Freeze) && !LastActionUsed(AID.Freeze) &&
                        (LastActionUsed(AID.Blizzard2) || MP < 10000))
                        QueueGCD(AID.Freeze, target, GCDPriority.SecondStep);
                    //Step 3 - swap from UI to AF
                    if (Unlocked(AID.Fire2) &&
                        MP >= 10000 &&
                        UmbralStacks == 3)
                        QueueGCD(AID.Fire2, target, GCDPriority.ThirdStep);
                }
                if (InAstralFire)
                {
                    //Step 1 - spam Fire 2
                    if (UmbralHearts > 1)
                        QueueGCD(AID.Fire2, target, GCDPriority.FirstStep);
                    //Step 2 - Flare
                    if (Unlocked(AID.Flare))
                    {
                        //first cast
                        if (UmbralHearts == 1)
                            QueueGCD(AID.Flare, target, GCDPriority.SecondStep);
                        //second cast
                        if (UmbralHearts == 0 &&
                            MP >= 800)
                            QueueGCD(AID.Flare, target, GCDPriority.ThirdStep);
                    }
                    //Step 3 - swap from AF to UI
                    if (Unlocked(AID.Blizzard2) &&
                        MP < 400)
                        QueueGCD(AID.Blizzard2, target, GCDPriority.FourthStep);
                }
            }
            if (Player.Level is >= 82 and <= 99)
            {
                if (InUmbralIce)
                {
                    //Step 1 - max stacks in UI
                    if (Unlocked(AID.HighBlizzard2) &&
                        UmbralStacks < 3)
                        QueueGCD(AID.HighBlizzard2, target, GCDPriority.FirstStep);
                    //Step 2 - Freeze
                    if (Unlocked(AID.Freeze) && !LastActionUsed(AID.Freeze) &&
                        (LastActionUsed(AID.HighBlizzard2) || MP < 10000))
                        QueueGCD(AID.Freeze, target, GCDPriority.SecondStep);
                    //Step 3 - swap from UI to AF
                    if (Unlocked(AID.HighFire2) &&
                        MP >= 10000 &&
                        UmbralStacks == 3)
                        QueueGCD(AID.HighFire2, target, GCDPriority.ThirdStep);
                }
                if (InAstralFire)
                {
                    //Step 1 - spam Fire 2
                    if (MP > 5500)
                        QueueGCD(AID.HighFire2, target, GCDPriority.FirstStep);
                    //Step 2 - Flare
                    if (Unlocked(AID.Flare))
                    {
                        //first cast
                        if (UmbralHearts == 1)
                            QueueGCD(AID.Flare, target, GCDPriority.SecondStep);
                        //second cast
                        if (UmbralHearts == 0 &&
                            MP >= 800)
                            QueueGCD(AID.Flare, target, GCDPriority.ThirdStep);
                    }
                    //Step 3 - swap from AF to UI
                    if (Unlocked(AID.HighBlizzard2) &&
                        MP < 400)
                        QueueGCD(AID.HighBlizzard2, target, GCDPriority.ThirdStep);
                }
            }
            if (Player.Level is 100)
            {
                if (InUmbralIce)
                {
                    //Step 1 - max stacks in UI
                    if (Unlocked(AID.HighBlizzard2) &&
                        UmbralStacks < 3)
                        QueueGCD(AID.HighBlizzard2, target, GCDPriority.FirstStep);
                    //Step 2 - Freeze
                    if (Unlocked(AID.Freeze) && !LastActionUsed(AID.Freeze) &&
                        (LastActionUsed(AID.HighBlizzard2) || MP < 10000))
                        QueueGCD(AID.Freeze, target, GCDPriority.SecondStep);
                    //Step 3 - swap from UI to AF
                    if (Unlocked(AID.HighFire2) &&
                        MP >= 10000 &&
                        UmbralStacks == 3)
                        QueueGCD(AID.HighFire2, target, GCDPriority.ThirdStep);
                }
                if (InAstralFire)
                {
                    //Step 1 - Flare
                    if (Unlocked(AID.Flare))
                    {
                        //first cast
                        if (UmbralHearts == 1)
                            QueueGCD(AID.Flare, target, GCDPriority.FirstStep);
                        //second cast
                        if (UmbralHearts == 0 &&
                            MP >= 800)
                            QueueGCD(AID.Flare, target, GCDPriority.SecondStep);
                    }
                    //Step 2 - Flare Star
                    if (AstralSoulStacks == 6) //if Astral Soul stacks are max
                        QueueGCD(AID.FlareStar, target, GCDPriority.ThirdStep); //Queue Flare Star
                    //Step 3 - swap from AF to UI
                    if (Unlocked(AID.HighBlizzard2) &&
                        MP < 400)
                        QueueGCD(AID.HighBlizzard2, target, GCDPriority.FourthStep);
                }
            }
        }
    }
    #endregion

    #region Cooldown Helpers
    private bool ShouldUseThunder(Actor? target, ThunderStrategy strategy) => strategy switch
    {
        ThunderStrategy.Thunder3 => Player.InCombat && target != null && hasThunderhead && ThunderLeft <= 3 && In25y(target),
        ThunderStrategy.Thunder6 => Player.InCombat && target != null && hasThunderhead && ThunderLeft <= 6 && In25y(target),
        ThunderStrategy.Thunder9 => Player.InCombat && target != null && hasThunderhead && ThunderLeft <= 9 && In25y(target),
        ThunderStrategy.Thunder0 => Player.InCombat && target != null && hasThunderhead && ThunderLeft is 0 && In25y(target),
        ThunderStrategy.Force => hasThunderhead,
        ThunderStrategy.Delay => false,
        _ => false
    };
    private bool ShouldSpendPolyglot(Actor? target, PolyglotStrategy strategy) => strategy switch
    {
        PolyglotStrategy.AutoSpendAll => target != null && UsePolyglots(),
        PolyglotStrategy.AutoHold1 => target != null && Polyglots > 1 && UsePolyglots(),
        PolyglotStrategy.AutoHold2 => target != null && Polyglots > 2 && UsePolyglots(),
        PolyglotStrategy.AutoHold3 => Player.InCombat && target != null && Polyglots == MaxPolyglots && (EnochianTimer <= 10000f || TotalCD(AID.Amplifier) < GCD),
        _ => false
    };
    private bool UsePolyglots()
    {
        if (Polyglots > 0)
        {
            if (Player.InCombat &&
                ((TotalCD(AID.Triplecast) < 5 || TotalCD(AID.Triplecast) <= 60 && TotalCD(AID.Triplecast) >= 65) && PlayerHasEffect(SID.LeyLines, 30) || //Triplecast prep
                TotalCD(AID.LeyLines) < 5 || TotalCD(AID.LeyLines) == 0 || TotalCD(AID.LeyLines) <= 125 && TotalCD(AID.LeyLines) >= 119 || //Ley Lines prep
                TotalCD(AID.Amplifier) < 0.6f || //Amplifier prep
                TotalCD(AID.Manafont) < 0.6f && MP < 1600)) //Manafont prep
                return true;
        }
        return false;
    }
    private bool ShouldUsePolyglot(Actor? target, PolyglotStrategy strategy) => strategy switch
    {
        PolyglotStrategy.AutoSpendAll or PolyglotStrategy.XenoSpendAll or PolyglotStrategy.FoulSpendAll => ShouldSpendPolyglot(target, PolyglotStrategy.AutoSpendAll),
        PolyglotStrategy.AutoHold1 or PolyglotStrategy.XenoHold1 or PolyglotStrategy.FoulHold1 => ShouldSpendPolyglot(target, PolyglotStrategy.AutoHold1),
        PolyglotStrategy.AutoHold2 or PolyglotStrategy.XenoHold2 or PolyglotStrategy.FoulHold2 => ShouldSpendPolyglot(target, PolyglotStrategy.AutoHold2),
        PolyglotStrategy.AutoHold3 or PolyglotStrategy.XenoHold3 or PolyglotStrategy.FoulHold3 => ShouldSpendPolyglot(target, PolyglotStrategy.AutoHold3),
        PolyglotStrategy.ForceXeno => canXeno,
        PolyglotStrategy.ForceFoul => canFoul,
        PolyglotStrategy.Delay => false,
        _ => false
    };
    private bool ShouldUseLeyLines(Actor? target, LeyLinesStrategy strategy) => strategy switch
    {
        LeyLinesStrategy.Automatic => Player.InCombat && target != null && canLL && CanWeaveIn,
        LeyLinesStrategy.Force => Player.InCombat && canLL,
        LeyLinesStrategy.Force1 => Player.InCombat && canLL && TotalCD(AID.LeyLines) < SpSGCDLength * 2,
        LeyLinesStrategy.ForceWeave => Player.InCombat && canLL && CanWeaveIn,
        LeyLinesStrategy.ForceWeave1 => Player.InCombat && canLL && CanWeaveIn && TotalCD(AID.LeyLines) < SpSGCDLength * 2,
        _ => false
    };
    private bool ShouldUseAmplifier(Actor? target, OffensiveStrategy strategy) => strategy switch
    {
        OffensiveStrategy.Automatic => Player.InCombat && target != null && canAmp && CanWeaveIn && Polyglots != MaxPolyglots,
        OffensiveStrategy.Force => canAmp,
        OffensiveStrategy.AnyWeave => canAmp && CanWeaveIn,
        OffensiveStrategy.EarlyWeave => canAmp && CanEarlyWeaveIn,
        OffensiveStrategy.LateWeave => canAmp && CanLateWeaveIn,
        _ => false
    };
    private bool ShouldUseRetrace(OffensiveStrategy strategy) => strategy switch
    {
        OffensiveStrategy.Force => canRetrace,
        OffensiveStrategy.AnyWeave => canRetrace && CanWeaveIn,
        OffensiveStrategy.EarlyWeave => canRetrace && CanEarlyWeaveIn,
        OffensiveStrategy.LateWeave => canRetrace && CanLateWeaveIn,
        _ => false
    };
    private bool ShouldUseBTL(OffensiveStrategy strategy) => strategy switch
    {
        OffensiveStrategy.Force => canBTL,
        OffensiveStrategy.AnyWeave => canBTL && CanWeaveIn,
        OffensiveStrategy.EarlyWeave => canBTL && CanEarlyWeaveIn,
        OffensiveStrategy.LateWeave => canBTL && CanLateWeaveIn,
        _ => false
    };
    private bool ShouldUseManafont(Actor? target, ManafontStrategy strategy) => strategy switch
    {
        ManafontStrategy.Automatic => Player.InCombat && target != null && canMF && CanWeaveIn && MP == 0,
        ManafontStrategy.Force => canMF,
        ManafontStrategy.ForceWeave => canMF && CanWeaveIn,
        ManafontStrategy.ForceEX => canMF,
        ManafontStrategy.ForceWeaveEX => canMF && CanWeaveIn,
        _ => false
    };
    private bool ShouldUseTriplecast(Actor? target, TriplecastStrategy strategy) => strategy switch
    {
        TriplecastStrategy.Automatic => Player.InCombat && target != null && canTC && CanWeaveIn && InAstralFire && PlayerHasEffect(SID.LeyLines, 30),
        TriplecastStrategy.Force => Player.InCombat && canTC,
        TriplecastStrategy.Force1 => Player.InCombat && canTC && TotalCD(AID.Triplecast) < SpSGCDLength * 2,
        TriplecastStrategy.ForceWeave => Player.InCombat && canTC && CanWeaveIn,
        TriplecastStrategy.ForceWeave1 => Player.InCombat && canTC && CanWeaveIn && TotalCD(AID.Triplecast) < SpSGCDLength * 2,
        _ => false
    };
    #endregion
}
