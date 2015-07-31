using System;
using System.Collections.Generic;

using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Content;
using Microsoft.Xna.Framework.Graphics;

namespace FragSharpFramework
{
    /// <summary>
    /// The set of possible ways that an expression can be translated to shader language.
    /// </summary>
    public enum TranslationType { ReplaceMember, ReplaceExpression, UnderscoreAppend, ReverseArguments };

    // NOTE: I'm the only one who finds the Vals class with attribute methods and a Vals attribute method confusing?
    public class Vals
    {
        [Vals(true, false)] public class BoolAttribute : Attribute { }
        public static readonly bool[] Bool = new bool[] { true, false };
    }

    /// <summary>
    /// ValsAttributes designated "specializations"; arguments which can be factored out of shaders to improve performance
    /// </summary>
    public class ValsAttribute : Attribute
    {
        public ValsAttribute(params int[] vals) { }
        public ValsAttribute(params float[] vals) { }
        public ValsAttribute(params bool[] vals) { }
    }

    public enum CastStyle { ExplicitCasts, ImplicitCast, NoCasts };
    public class CopyAttribute : Attribute
    {
        public CopyAttribute(Type type) { }
        public CopyAttribute(Type type, CastStyle style) { }
    }

    public class KeepInCopy : Attribute { }

    public enum Special { rgba_hex, rgb_hex }
    public class SpecialAttribute : Attribute
    {
        public SpecialAttribute(Special name) { }
    }

    /// <summary>
    /// Base class for shader translation attributes.
    /// </summary>
    public class __slAttribute : Attribute
    {
        public __slAttribute() { }
        public __slAttribute(string translation) { }
        public __slAttribute(string translation, TranslationType translation_type) { }
    }

    /// <summary>
    /// Attributes which define how an expression gets translated to HLSL
    /// </summary>
    public class HlslAttribute : __slAttribute
    {
        public HlslAttribute() : base() { }
        public HlslAttribute(string translation) : base(translation) { }
        public HlslAttribute(string translation, TranslationType translation_type) : base(translation, translation_type) { }
    }

    /// <summary>
    /// Attributes which define how an expression gets translated to GLSL
    /// </summary>
    public class GlslAttribute : __slAttribute
    {
        public GlslAttribute() : base() { }
        public GlslAttribute(string translation) : base(translation) { }
        public GlslAttribute(string translation, TranslationType translation_type) : base(translation, translation_type) { }
    }

    public class POSITION0Attribute : Attribute { }
    public class COLOR0Attribute : Attribute { }
    public class TEXCOORD0Attribute : Attribute { }

    public class VertexShaderAttribute : Attribute { }
    public class FragmentShaderAttribute : Attribute { }
}