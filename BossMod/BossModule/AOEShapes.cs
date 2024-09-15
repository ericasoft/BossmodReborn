﻿namespace BossMod;

public abstract record class AOEShape
{
    public abstract bool Check(WPos position, WPos origin, Angle rotation);
    public abstract void Draw(MiniArena arena, WPos origin, Angle rotation, uint color = 0);
    public abstract void Outline(MiniArena arena, WPos origin, Angle rotation, uint color = 0);
    public abstract void Draw(MiniArena arena, WPos origin, Angle rotation, uint color = 0);
    public abstract void Outline(MiniArena arena, WPos origin, Angle rotation, uint color = 0);
    public abstract Func<WPos, float> Distance(WPos origin, Angle rotation);

    public bool Check(WPos position, Actor? origin)
    {
        return origin != null && Check(position, origin.Position, origin.Rotation);
    }

    public void Draw(MiniArena arena, Actor? origin, uint color = 0)
    {
        if (origin != null)
            Draw(arena, origin.Position, origin.Rotation, color == 0 ? Colors.AOE : color);
    }

    public void Outline(MiniArena arena, Actor? origin, uint color = 0)
    {
        if (origin != null)
            Outline(arena, origin.Position, origin.Rotation, color == 0 ? Colors.Danger : color);
    }
}

public sealed record class AOEShapeCone(float Radius, Angle HalfAngle, Angle DirectionOffset = default, bool InvertForbiddenZone = false) : AOEShape
{
    public override string ToString() => $"Cone: r={Radius:f3}, angle={HalfAngle * 2}, off={DirectionOffset}, ifz={InvertForbiddenZone}";
    public override bool Check(WPos position, WPos origin, Angle rotation) => position.InCircleCone(origin, Radius, rotation + DirectionOffset, HalfAngle);
    public override void Draw(MiniArena arena, WPos origin, Angle rotation, uint color = 0) => arena.ZoneCone(origin, 0, Radius, rotation + DirectionOffset, HalfAngle, color);
    public override void Outline(MiniArena arena, WPos origin, Angle rotation, uint color = 0) => arena.AddCone(origin, Radius, rotation + DirectionOffset, HalfAngle, color);
    public override Func<WPos, float> Distance(WPos origin, Angle rotation)
    {
        return !InvertForbiddenZone
            ? ShapeDistance.Cone(origin, Radius, rotation + DirectionOffset, HalfAngle)
            : ShapeDistance.InvertedCone(origin, Radius, rotation + DirectionOffset, HalfAngle);
    }
}

public sealed record class AOEShapeCircle(float Radius, bool InvertForbiddenZone = false) : AOEShape
{
    public override string ToString() => $"Circle: r={Radius:f3}, ifz={InvertForbiddenZone}";
    public override bool Check(WPos position, WPos origin, Angle rotation = new()) => position.InCircle(origin, Radius);
    public override void Draw(MiniArena arena, WPos origin, Angle rotation = new(), uint color = 0) => arena.ZoneCircle(origin, Radius, color);
    public override void Outline(MiniArena arena, WPos origin, Angle rotation = new(), uint color = 0) => arena.AddCircle(origin, Radius, color);
    public override Func<WPos, float> Distance(WPos origin, Angle rotation)
    {
        return !InvertForbiddenZone
            ? ShapeDistance.Circle(origin, Radius)
            : ShapeDistance.InvertedCircle(origin, Radius);
    }
}

public sealed record class AOEShapeDonut(float InnerRadius, float OuterRadius, bool InvertForbiddenZone = false) : AOEShape
{
    public override string ToString() => $"Donut: r={InnerRadius:f3}-{OuterRadius:f3}, ifz={InvertForbiddenZone}";
    public override bool Check(WPos position, WPos origin, Angle rotation = new()) => position.InDonut(origin, InnerRadius, OuterRadius);
    public override void Draw(MiniArena arena, WPos origin, Angle rotation = new(), uint color = 0) => arena.ZoneDonut(origin, InnerRadius, OuterRadius, color == 0 ? Colors.AOE : color);
    public override void Outline(MiniArena arena, WPos origin, Angle rotation = new(), uint color = 0)
    {
        arena.AddCircle(origin, InnerRadius, colors);
        arena.AddCircle(origin, OuterRadius, colors);
    }
    public override Func<WPos, float> Distance(WPos origin, Angle rotation)
    {
        return !InvertForbiddenZone
            ? ShapeDistance.Donut(origin, InnerRadius, OuterRadius)
            : ShapeDistance.InvertedDonut(origin, InnerRadius, OuterRadius);
    }
}

public sealed record class AOEShapeDonutSector(float InnerRadius, float OuterRadius, Angle HalfAngle, Angle DirectionOffset = default, bool InvertForbiddenZone = false) : AOEShape
{
    public override string ToString() => $"Donut sector: r={InnerRadius:f3}-{OuterRadius:f3}, angle={HalfAngle * 2}, off={DirectionOffset}, ifz={InvertForbiddenZone}";
    public override bool Check(WPos position, WPos origin, Angle rotation) => position.InDonutCone(origin, InnerRadius, OuterRadius, rotation + DirectionOffset, HalfAngle);
    public override void Draw(MiniArena arena, WPos origin, Angle rotation, uint color = 0) => arena.ZoneCone(origin, InnerRadius, OuterRadius, rotation + DirectionOffset, HalfAngle, color);
    public override void Outline(MiniArena arena, WPos origin, Angle rotation, uint color = 0) => arena.AddDonutCone(origin, InnerRadius, OuterRadius, rotation + DirectionOffset, HalfAngle, color);
    public override Func<WPos, float> Distance(WPos origin, Angle rotation)
    {
        return !InvertForbiddenZone
            ? ShapeDistance.DonutSector(origin, InnerRadius, OuterRadius, rotation + DirectionOffset, HalfAngle)
            : ShapeDistance.InvertedDonutSector(origin, InnerRadius, OuterRadius, rotation + DirectionOffset, HalfAngle);
    }
}

public sealed record class AOEShapeRect(float LengthFront, float HalfWidth, float LengthBack = 0, Angle DirectionOffset = default, bool InvertForbiddenZone = false) : AOEShape
{
    public override string ToString() => $"Rect: l={LengthFront:f3}+{LengthBack:f3}, w={HalfWidth * 2}, off={DirectionOffset}, ifz={InvertForbiddenZone}";
    public override bool Check(WPos position, WPos origin, Angle rotation) => position.InRect(origin, rotation + DirectionOffset, LengthFront, LengthBack, HalfWidth);
    public override void Draw(MiniArena arena, WPos origin, Angle rotation, uint color = 0) => arena.ZoneRect(origin, rotation + DirectionOffset, LengthFront, LengthBack, HalfWidth, color);
    public override void Outline(MiniArena arena, WPos origin, Angle rotation, uint color = 0) => arena.AddRect(origin, (rotation + DirectionOffset).ToDirection(), LengthFront, LengthBack, HalfWidth, color);
    public override Func<WPos, float> Distance(WPos origin, Angle rotation)
    {
        return !InvertForbiddenZone
            ? ShapeDistance.Rect(origin, rotation + DirectionOffset, LengthFront, LengthBack, HalfWidth)
            : ShapeDistance.InvertedRect(origin, rotation + DirectionOffset, LengthFront, LengthBack, HalfWidth);
    }
}

public sealed record class AOEShapeCross(float Length, float HalfWidth, Angle DirectionOffset = default, bool InvertForbiddenZone = false) : AOEShape
{
    public override string ToString() => $"Cross: l={Length:f3}, w={HalfWidth * 2}, off={DirectionOffset}, ifz={InvertForbiddenZone}";
    public override bool Check(WPos position, WPos origin, Angle rotation) => position.InRect(origin, rotation + DirectionOffset, Length, Length, HalfWidth) || position.InRect(origin, rotation + DirectionOffset, HalfWidth, HalfWidth, Length);
    public override void Draw(MiniArena arena, WPos origin, Angle rotation, uint color = 0) => arena.ZonePoly((GetType(), origin, rotation + DirectionOffset, Length, HalfWidth), ContourPoints(origin, rotation), color);
    public override void Outline(MiniArena arena, WPos origin, Angle rotation, uint color = 0)
    {
        foreach (var p in ContourPoints(origin, rotation))
            arena.PathLineTo(p);
        MiniArena.PathStroke(true, color == 0 ? Colors.Danger : color);
    }

    private IEnumerable<WPos> ContourPoints(WPos origin, Angle rotation, float offset = 0)
    {
        var dx = (rotation + DirectionOffset).ToDirection();
        var dy = dx.OrthoL();
        var dx1 = dx * (Length + offset);
        var dx2 = dx * (HalfWidth + offset);
        var dy1 = dy * (Length + offset);
        var dy2 = dy * (HalfWidth + offset);
        yield return origin + dx1 - dy2;
        yield return origin + dx2 - dy2;
        yield return origin + dx2 - dy1;
        yield return origin - dx2 - dy1;
        yield return origin - dx2 - dy2;
        yield return origin - dx1 - dy2;
        yield return origin - dx1 + dy2;
        yield return origin - dx2 + dy2;
        yield return origin - dx2 + dy1;
        yield return origin + dx2 + dy1;
        yield return origin + dx2 + dy2;
        yield return origin + dx1 + dy2;
    }

    public override Func<WPos, float> Distance(WPos origin, Angle rotation)
    {
        return !InvertForbiddenZone
            ? ShapeDistance.Cross(origin, rotation + DirectionOffset, Length, HalfWidth)
            : ShapeDistance.InvertedCross(origin, rotation + DirectionOffset, Length, HalfWidth);
    }
}

public sealed record class AOEShapeTriCone(float SideLength, Angle HalfAngle, Angle DirectionOffset = default, bool InvertForbiddenZone = false) : AOEShape
{
    public override string ToString() => $"TriCone: side={SideLength:f3}, angle={HalfAngle * 2}, off={DirectionOffset}, ifz={InvertForbiddenZone}";
    public override bool Check(WPos position, WPos origin, Angle rotation) => position.InTri(origin, origin + SideLength * (rotation + DirectionOffset + HalfAngle).ToDirection(), origin + SideLength * (rotation + DirectionOffset - HalfAngle).ToDirection());
    public override void Draw(MiniArena arena, WPos origin, Angle rotation, uint color = 0) => arena.ZoneTri(origin, origin + SideLength * (rotation + DirectionOffset + HalfAngle).ToDirection(), origin + SideLength * (rotation + DirectionOffset - HalfAngle).ToDirection(), color);
    public override void Outline(MiniArena arena, WPos origin, Angle rotation, uint color = 0) => arena.AddTriangle(origin, origin + SideLength * (rotation + DirectionOffset + HalfAngle).ToDirection(), origin + SideLength * (rotation + DirectionOffset - HalfAngle).ToDirection(), color);

    public override Func<WPos, float> Distance(WPos origin, Angle rotation)
    {
        var direction1 = SideLength * (rotation + DirectionOffset + HalfAngle).ToDirection();
        var direction2 = SideLength * (rotation + DirectionOffset - HalfAngle).ToDirection();
        var shape = new RelTriangle(default, direction1, direction2);
        return !InvertForbiddenZone ? ShapeDistance.Tri(origin, shape) : ShapeDistance.InvertedTri(origin, shape);
    }
}

public enum OperandType
{
    Union,
    Xor,
    Intersection,
    Difference
}

// shapes1 for unions, shapes 2 for shapes for XOR/intersection with shapes1, differences for shapes that get subtracted after previous operations
public sealed record class AOEShapeCustom(IEnumerable<Shape> Shapes1, IEnumerable<Shape>? DifferenceShapes = null, IEnumerable<Shape>? Shapes2 = null, bool InvertForbiddenZone = false, OperandType Operand = OperandType.Union) : AOEShape
{
    private static readonly Dictionary<string, RelSimplifiedComplexPolygon> _polygonCache = [];
    private static readonly Queue<string> _polygonCacheQueue = new();
    private readonly Dictionary<(string, WPos, WPos, Angle), bool> _checkCache = [];
    private static readonly Dictionary<(string, WPos, Angle, bool), Func<WPos, float>> _distanceFuncCache = [];
    private readonly string sha512key = CreateCacheKey(Shapes1, Shapes2 ?? [], DifferenceShapes ?? [], Operand);

    public override string ToString() => $"Custom AOE shape: sha512key={sha512key}, ifz={InvertForbiddenZone}";

    private RelSimplifiedComplexPolygon GetCombinedPolygon(WPos origin)
    {
        if (_polygonCache.TryGetValue(sha512key, out var cachedResult))
            return cachedResult;

        var shapes1 = CreateOperandFromShapes(Shapes1, origin);
        var shapes2 = CreateOperandFromShapes(Shapes2, origin);
        var differenceOperands = CreateOperandFromShapes(DifferenceShapes, origin);

        var clipper = new PolygonClipper();
        var combinedShapes = Operand switch
        {
            OperandType.Xor => clipper.Xor(shapes1, shapes2),
            OperandType.Intersection => clipper.Intersect(shapes1, shapes2),
            _ => null
        };

        var finalResult = combinedShapes != null
            ? clipper.Difference(new PolygonClipper.Operand(combinedShapes), differenceOperands)
            : clipper.Difference(shapes1, differenceOperands);

        AddToPolygonCache(sha512key, finalResult);
        return finalResult;
    }

    private PolygonClipper.Operand CreateOperandFromShapes(IEnumerable<Shape>? shapes, WPos origin)
    {
        var operand = new PolygonClipper.Operand();
        if (shapes != null)
            foreach (var shape in shapes)
                operand.AddPolygon(shape.ToPolygon(origin));
        return operand;
    }

    public override bool Check(WPos position, WPos origin, Angle rotation)
    {
        var cacheKey = (sha512key, position, origin, rotation);
        if (_checkCache.TryGetValue(cacheKey, out var cachedResult))
            return cachedResult;

        var combinedPolygon = GetCombinedPolygon(origin);
        var relativePosition = position - origin;
        var result = combinedPolygon.Contains(new WDir(relativePosition.X, relativePosition.Z));
        AddToCheckCache(cacheKey, result);
        return result;
    }

    private static string CreateCacheKey(IEnumerable<Shape> shapes1, IEnumerable<Shape> shapes2, IEnumerable<Shape> differenceShapes, OperandType operand)
    {
        var shapes1Key = string.Join(",", shapes1.Select(s => s.ComputeHash()));
        var shapes2Key = string.Join(",", shapes2.Select(s => s.ComputeHash()));
        var differenceKey = string.Join(",", differenceShapes.Select(s => s.ComputeHash()));
        var combinedKey = $"{shapes1Key}|{shapes2Key}|{differenceKey}|{operand}";
        return Shape.ComputeSHA512(combinedKey);
    }

    public override void Draw(MiniArena arena, WPos origin, Angle rotation, uint color = 0)
    {
        arena.ZoneRelPoly(sha512key, GetCombinedPolygon(origin), color);
    }

    public override void Outline(MiniArena arena, WPos origin, Angle rotation, uint color = 0)
    {
        var combinedPolygon = GetCombinedPolygon(origin);
        foreach (var part in combinedPolygon.Parts)
        {
            var exteriorEdges = part.ExteriorEdges.ToList();
            for (var i = 0; i < exteriorEdges.Count; i++)
            {
                var (start, end) = exteriorEdges[i];
                arena.PathLineTo(origin + start);
                if (i != exteriorEdges.Count - 1)
                    arena.PathLineTo(origin + end);
            }
            MiniArena.PathStroke(true, color == 0 ? Colors.Danger : color);

            foreach (var holeIndex in part.Holes)
            {
                var interiorEdges = part.InteriorEdges(holeIndex).ToList();
                for (var i = 0; i < interiorEdges.Count; i++)
                {
                    var (start, end) = interiorEdges[i];
                    arena.PathLineTo(origin + start);
                    if (i != interiorEdges.Count - 1)
                        arena.PathLineTo(origin + end);
                }
                MiniArena.PathStroke(true, color == 0 ? Colors.Danger : color);
            }
        }
    }

    public override Func<WPos, float> Distance(WPos origin, Angle rotation)
    {
        var cacheKey = (sha512key, origin, rotation, InvertForbiddenZone);
        if (_distanceFuncCache.TryGetValue(cacheKey, out var cachedFunc))
            return cachedFunc;

        var result = InvertForbiddenZone ? RelPolygonWithHoles.InvertedPolygonWithHoles(origin, GetCombinedPolygon(origin)) : RelPolygonWithHoles.PolygonWithHoles(origin, GetCombinedPolygon(origin));
        _distanceFuncCache[cacheKey] = result;
        return result;
    }

    private void AddToPolygonCache(string key, RelSimplifiedComplexPolygon polygon)
    {
        _polygonCache[key] = polygon;
        _polygonCacheQueue.Enqueue(key);
        if (_polygonCache.Count > 250)
            _polygonCache.Remove(_polygonCacheQueue.Dequeue());
    }

    private void AddToCheckCache((string, WPos, WPos, Angle) key, bool result)
    {
        if (_checkCache.Count > 2500)
            _checkCache.Clear();
        _checkCache[key] = result;
    }
}
