namespace BossMod.Global.MaskedCarnivale;

public static class Layouts
{
    public static readonly WPos ArenaCenter = new(100f, 100f);
    private static readonly Angle a45 = 45f.Degrees();
    private static readonly Polygon[] _circleBig = [new Polygon(ArenaCenter, 24.5f * CosPI.Pi32th, 32)];
    private static readonly Rectangle[] walls = [new(new(90f, 94.75f), 5.5f, 0.75f), new(new(94.75f, 91.75f), 0.75f, 3.25f),
    new(new(110f, 94.75f), 5.5f, 0.75f), new(new(105.25f, 91.75f), 0.75f, 3.25f)];

    private const float sideLength = 2.4f;
    private static readonly Square[] squares = [new(new(110f, 110f), sideLength, a45), new(new(90f, 110f), sideLength, a45),
    new(new(110f, 90f), sideLength, a45), new(new(90f, 90f), sideLength, a45)];

    public static readonly ArenaBoundsComplex Layout4Quads = new(_circleBig, squares);
    public static readonly ArenaBoundsComplex Layout2Corners = new(_circleBig, walls);
    public static readonly ArenaBoundsComplex LayoutBigQuad = new(_circleBig, [new Square(ArenaCenter, 5.4f, a45)]);
    public static readonly ArenaBoundsComplex CircleSmall = new([new Polygon(ArenaCenter, 16.01379f, 32)]) { IsCircle = true };
    public static readonly ArenaBoundsComplex CircleBig = new(_circleBig) { IsCircle = true };
}
