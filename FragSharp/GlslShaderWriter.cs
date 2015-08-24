using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;
using Roslyn.Compilers.CSharp;

using FragSharpFramework;

// TODO: This file needs to actually be ported from Hlsl to GLSL

namespace FragSharp
{

    /// <summary>
    /// This class is responsible for generating the C# boilerplate which will be used to compile the generated FragSharp shaders.
    /// </summary>
    internal class GlslShaderWriter : GlslWriter
    {
        public GlslShaderWriter(Dictionary<SyntaxTree, SemanticModel> models, Compilation compilation, Dictionary<Symbol, string> specialization)
            : base(models, compilation)
        {
            this.specialization = specialization;
        }

        Dictionary<Symbol, string> specialization;

        int SamplerNumber = 0;

        /// <summary>
        /// Enumerates the possible types of methods which can be currently compiling: VertexeMethod, FragmentMethod, or None
        /// </summary>
        enum Compiling { None, VertexMethod, FragmentMethod };
        /// <summary>
        /// What type of method is currently compiling. Defuaults to None.
        /// </summary>
        Compiling CurrentMethod = Compiling.None;

        public string CompileShader(NamedTypeSymbol Symbol, MethodDeclarationSyntax vertex_method, MethodDeclarationSyntax fragment_method)
        {
            ClearString();
            
            // Declare samplers and other relevant structures needed for the Fragment Shader
            Write(SpaceFormat(FileBegin));
            EndLine();
            
            WriteLine();

            WriteLine(VertexMethodParameters);
            CompileVertexSignature(vertex_method);
            EndLine();

            WriteLine(FragmentMethodParameters);
            var LocalFragmentLookup = CompileFragmentSignature(fragment_method);
            EndLine();

            // Referenced foreign variables
            Write(SpaceFormat(ReferencedForeignVarsPreamble));
            EndLine();
            Write("<$0$>"); // This is where we will insert referenced foreign variables.

            WriteLine();

            // Referenced methods
            Write(SpaceFormat(ReferencedMethodsPreamble));
            EndLine();
            Write("<$1$>"); // This is where we will insert referenced methods.
            
            WriteLine();

            // Fragment Shader method
            UseLocalSymbolMap(LocalFragmentLookup);

            CurrentMethod = Compiling.FragmentMethod;
            Write(FragmentShaderBegin, Tab, LineBreak, VertexToPixelDecl);
            EndLine();
            var PrevIndent = Indent();
            FunctionParameterPrefix = FragmentShaderParameterPrefix;
            CompileStatement(fragment_method.Body);
            RestoreIndent(PrevIndent);
            Write(SpaceFormat(FragmentShaderEnd));
            EndLine();

            UseLocalSymbolMap(null);

            WriteLine();

            // Vertex Shader method
            CurrentMethod = Compiling.VertexMethod;
            Write(SpaceFormat(VertexShaderBegin));
            EndLine();
            PrevIndent = Indent();
            FunctionParameterPrefix = VertexShaderParameterPrefix;
            CompileStatement(vertex_method.Body);
            RestoreIndent(PrevIndent);
            Write(SpaceFormat(VertexShaderEnd));
            EndLine();

            WriteLine();

            // We must wait until after compiling the shader to know which methods that shader references.
            string methods = GetReferencedMethods();

            // We must wait until after compiling the shader to know which foreign variables that shader references.
            string foreign_vars = GetForeignVars();
            
            // Now get the full string written so far and insert the referenced methods.
            string fragment = GetString();
            fragment = SpecialFormat(fragment, foreign_vars, methods);

            return fragment;
        }

        /// <summary>
        /// Loops over ReferencedForeignVars, calling CompileSamplerParameter_Foreign on each. Returns string of these compilations.
        /// </summary>
        /// <returns></returns>
        virtual protected string GetForeignVars()
        {
            string foreign_vars = string.Empty;

            if (ReferencedForeignVars.Count == 0)
                return foreign_vars;

            var hold_writer = writer;
            writer = new StringWriter();

            var last = ReferencedForeignVars.Last();
            foreach (var foreign_var in ReferencedForeignVars)
            {
                //if shader
                CompileSamplerParameter_Foreign(foreign_var);
                WriteLine();
                // else
                //var param = new Param(GetType(foreign_var).Name, type, name, mapped_name, Param.ParamType.VertexParam);
                //Params.Add(param);
                //AllParams.Add(param);
            }

            foreign_vars = writer.ToString();
            writer = hold_writer;

            return foreign_vars;
        }

        /// <summary>
        /// For the given symbol and specializations, writes the namespace and makes call to generate class boilerplate string.
        /// </summary>
        /// <param name="Symbol"></param>
        /// <param name="Specializations"></param>
        /// <returns></returns>
        public string CompileShaderBoilerplate(NamedTypeSymbol Symbol, List<Dictionary<Symbol, string>> Specializations)
        {
            ClearString();

            WriteLine("namespace {0}", Symbol.ContainingNamespace);
            WriteLine("{");

            var PrevIndent1 = Indent();

            WriteBoilerplateClass(Symbol, Specializations);

            RestoreIndent(PrevIndent1);

            WriteLine("}");
            WriteLine();

            string boilerplate = GetString();
            return boilerplate;
        }

        const string ApplyName = "Apply", UsingName = "Using";

        /// <summary>
        /// For the given symbol and specializations, generates the class declarations, signatures, and calls to apply functions for the shaders
        /// </summary>
        /// <param name="symbol"></param>
        /// <param name="Specializations"></param>
        void WriteBoilerplateClass(NamedTypeSymbol symbol, List<Dictionary<Symbol, string>> Specializations)
        {
            WriteLine("public partial class {0}", symbol.Name);
            WriteLine("{");

            var PrevIndent = Indent();

            // If there are no specializations (count == 1) then we only need a single Effect.
            if (Specializations.Count == 1)
            {
                WriteLine("public static Effect CompiledEffect;");
            }
            else
            {
                // Otherwise we need an Effect for each specialization.
                foreach (var specialization in Specializations)
                {
                    WriteLine("public static Effect CompiledEffect{0};", ShaderClass.SpecializationVarSuffix(specialization));
                }
            }

            WriteLine();

            WriteBoilerplateSignature(ApplyName, "RenderTarget2D Output, Color Clear");
            WriteBoilerplateApplyFunc("Output", "Clear");
            WriteBoilerplateSignature(ApplyName, "RenderTarget2D Output");
            WriteBoilerplateApplyFunc("Output", "Color.Transparent");

            WriteBoilerplateSignature(UsingName, "RenderTarget2D Output, Color Clear");
            WriteBoilerplateUsingFuncOverload("Output", "Clear");
            WriteBoilerplateSignature(UsingName, "RenderTarget2D Output");
            WriteBoilerplateUsingFuncOverload("Output", "Color.Transparent");

            WriteBoilerplateSignature(UsingName);
            WriteBoilerplateUsingFunc(Specializations);

            RestoreIndent(PrevIndent);

            WriteLine("}");    
        }

        /// <summary>
        /// Given a function name and optional extra parameters, this writes the function signature.
        /// </summary>
        /// <param name="FunctionName"></param>
        /// <param name="ExtraParams"></param>
        void WriteBoilerplateSignature(string FunctionName, string ExtraParams = null)
        {
            BeginLine("public static void {0}(", FunctionName);

            foreach (var param in NonForeignParams)
            {
                Write("{0} {1}", param.TypeName, param.Name);

                if (ExtraParams != null || param != NonForeignParams.Last())
                {
                    Write(",{0}", Space);
                }
            }

            if (ExtraParams != null)
            {
                Write(ExtraParams);
            }

            EndLine(")");
        }

        /// <summary>
        /// Writes an invocation of UsingName, wrapping parameters from NonForeignParams.
        /// </summary>
        void WriteInvokeUsing()
        {
            BeginLine("{0}(", UsingName);

            foreach (var param in NonForeignParams)
            {
                Write(param.Name);

                if (param != NonForeignParams.Last())
                {
                    Write(",{0}", Space);
                }
            }

            EndLine(");");
        }

        /// <summary>
        /// This makes a call to write "SetRenderTarget" and "Clear" calls, then writes invocation of UsingName propety on all NonForeignParams. Also writes call to "GridHelper.DrawGrid".
        /// </summary>
        /// <param name="SetRenderTarget"></param>
        /// <param name="ClearTarget"></param>
        void WriteBoilerplateApplyFunc(string SetRenderTarget, string ClearTarget)
        {
            WriteLine("{");
            var PrevIndent = Indent();

            CodeFor_SetRender_Clear(SetRenderTarget, ClearTarget);

            WriteInvokeUsing();

            WriteLine("GridHelper.DrawGrid();");

            RestoreIndent(PrevIndent);
            WriteLine("}");    
        }

        /// <summary>
        /// Given a render target and/or a clear target, this writes "SetRenderTarget" and "Clear" calls (from GridHelper.GraphicsDevice)
        /// </summary>
        /// <param name="SetRenderTarget"></param>
        /// <param name="ClearTarget"></param>
        private void CodeFor_SetRender_Clear(string SetRenderTarget, string ClearTarget)
        {
            if (SetRenderTarget != null)
                WriteLine("GridHelper.GraphicsDevice.SetRenderTarget({0});", SetRenderTarget);

            if (ClearTarget != null)
                WriteLine("GridHelper.GraphicsDevice.Clear({0});", ClearTarget);
        }

        /// <summary>
        /// This makes a call to write "SetRenderTarget" and "Clear" calls, then writes invocation of UsingName propety on all NonForeignParams. Also writes call to "GridHelper.DrawGrid".
        /// </summary>
        /// <param name="SetRenderTarget"></param>
        /// <param name="ClearTarget"></param>
        void WriteBoilerplateUsingFuncOverload(string SetRenderTarget, string ClearTarget)
        {
            WriteLine("{");
            var PrevIndent = Indent();

            CodeFor_SetRender_Clear(SetRenderTarget, ClearTarget);

            WriteInvokeUsing();

            RestoreIndent(PrevIndent);
            WriteLine("}");
        }

        void WriteBoilerplateUsingFunc(List<Dictionary<Symbol, string>> Specializations)
        {
            WriteLine("{");
            var PrevIndent = Indent();

            WriteBoilerplateEffectChoice(Specializations);

            foreach (var param in Params)
            {
                if (param.MappedType == "shader")
                {
                    WriteLine("CompiledEffect.Parameters[\"{0}_Texture\"].SetValue(FragSharpMarshal.Marshal({1}));", param.MappedName, param.Name);
                    WriteLine("CompiledEffect.Parameters[\"{0}_{2}\"].SetValue(FragSharpMarshal.Marshal(vec({1}.Width, {1}.Height)));", param.MappedName, param.Name, Sampler.SizeSuffix);
                    WriteLine("CompiledEffect.Parameters[\"{0}_{2}\"].SetValue(FragSharpMarshal.Marshal(1.0f / vec({1}.Width, {1}.Height)));", param.MappedName, param.Name, Sampler.DxDySuffix);
                }
                else
                {
                    WriteLine("CompiledEffect.Parameters[\"{0}\"].SetValue(FragSharpMarshal.Marshal({1}));", param.MappedName, param.Name);
                }
            }

            WriteLine("CompiledEffect.CurrentTechnique.Passes[0].Apply();");

            RestoreIndent(PrevIndent);
            WriteLine("}");
        }

        /// <summary>
        /// Given a specialization, this writes the if/else series which determines which specialized shader should be called.
        /// </summary>
        /// <param name="Specializations"></param>
        void WriteBoilerplateEffectChoice(List<Dictionary<Symbol, string>> Specializations)
        {
            if (Specializations.Count > 1)
            {
                WriteLine("Effect CompiledEffect = null;");
                WriteLine();

                foreach (var specialization in Specializations)
                {
                    WriteLine("{2}if ({1}) CompiledEffect = CompiledEffect{0};",
                        ShaderClass.SpecializationVarSuffix(specialization),
                        SpecializationEquality(specialization),
                        specialization == Specializations.First() ? "" : "else ");
                }
                WriteLine();

                WriteLine("if (CompiledEffect == null) throw new Exception(\"Parameters do not match any specified specialization.\");");
                WriteLine();
            }
        }

        /// <summary>
        /// Given a specialization, this generates equality statements for comparing argument values with specialization values. Important when comparing floating point numbers with epsilon precision.
        /// </summary>
        /// <param name="specialization"></param>
        /// <returns></returns>
        string SpecializationEquality(Dictionary<Symbol, string> specialization)
        {
            string equality = "";
            foreach (var variable in specialization)
            {
                if (variable.Key.ToString() == "bool")
                    equality += string.Format("{1}{0}=={0}{2}", Space, variable.Key.Name, variable.Value);
                else
                    equality += string.Format("abs((float)({1}{0}-{0}{2})){0}<{0}{3}", Space, variable.Key.Name, variable.Value, eps);

                if (variable.Key != specialization.Last().Key)
                    equality += string.Format("{0}&&{0}", Space);
            }

            return equality;
        }

        /// <summary>
        /// Compiles the return statements of a fragment. If Compiling a VertexFragment or None, calls base.CompileReturnStatement for string.
        /// </summary>
        /// <param name="statement"></param>
        override protected void CompileReturnStatement(ReturnStatementSyntax statement)
        {
            if (CurrentMethod == Compiling.FragmentMethod)
            {
                BeginLine("gl_FragColor{0}={0}", Space);
                CompileExpression(statement.Expression);
                EndLine(";");
            }
            if (CurrentMethod == Compiling.VertexMethod)
            {
                BeginLine("CarryOver{0}={0}", Space);
                CompileExpression(statement.Expression);
                EndLine(";");
            }
            else
            {
                base.CompileReturnStatement(statement);
            }
        }

        string SpaceFormat(string s)
        {
            return string.Format(s, Tab, LineBreak);
        }

        /// <summary>
        /// Class for parameters to vertex and fragment shaders.
        /// </summary>
        class Param
        {
            public enum ParamType { VertexParam, FragmentParam };

            public string TypeName, MappedType, Name, MappedName;
            public ParamType Type;

            public bool Foreign;

            public Param(string TypeName, string MappedType, string Name, string MappedName, ParamType Type)
            {
                this.TypeName = TypeName;
                this.MappedType = MappedType;
                this.Name = Name;
                this.MappedName = MappedName;
                this.Type = Type;
            }
        }

        /// <summary>
        /// Parameters that are used in the vertex/fragment shader, NOT including parameters factored out as constant specializations.
        /// </summary>
        List<Param> Params = new List<Param>();

        /// <summary>
        /// Parameters that are used in the vertex/fragment shader, INCLUDING parameters factored out as constant specializations.
        /// </summary>
        List<Param> AllParams = new List<Param>();

        IEnumerable<Param> NonForeignParams
        {
            get
            {
                return AllParams.Where(p => !p.Foreign);
            }
        }

        /// <summary>
        /// Given a method (assumed to be a vertex shader), this loops over the method's parameters and compiles them.
        /// </summary>
        /// <param name="method"></param>
        void CompileVertexSignature(MethodDeclarationSyntax method)
        {
            var ParameterList = method.ParameterList.Parameters;
            if (ParameterList.Count == 0) return;

            var first = ParameterList.First();
            var last = ParameterList.Last();
            foreach (var parameter in ParameterList)
            {
                if (parameter == first) continue;

                CompileVertexParameter(parameter);
                EndLine();
            }
        }

        /// <summary>
        /// Given a method (assumed to be a fragment shader), loops over methods parameters. Adds first parameter to SignatureMap because it must be a VertexOut which maps to "psin". Compiles all parameters and writes those which are not specialized.
        /// </summary>
        /// <param name="method"></param>
        /// <returns></returns>
        Dictionary<Symbol, string> CompileFragmentSignature(MethodDeclarationSyntax method)
        {
            var SignatureMap = new Dictionary<Symbol, string>(specialization);

            var ParameterList = method.ParameterList.Parameters;
            if (ParameterList.Count == 0) return SignatureMap;

            var first = ParameterList.First();
            var last  = ParameterList.Last();
            foreach (var parameter in ParameterList)
            {
                var symbol = GetSymbol(parameter);

                // The first parameter must be a VertexOut, which maps to "psin"
                if (parameter == first)
                {
                    SignatureMap.Add(symbol, "psin");
                    continue;
                }

                // Skip specialization values
                bool specialized = SignatureMap.ContainsKey(symbol);

                CompileFragmentParameter(parameter, specialized);

                if (!specialized)
                {
                    EndLine();

                    if (parameter != last)
                    {
                        WriteLine();
                    }
                }
            }

            return SignatureMap;
        }

        const string VertexShaderParameterPrefix = "vs_param_";
        const string FragmentShaderParameterPrefix = "fs_param_";

        /// <summary>
        /// Given a parameter (assumed to be to a vertex shader), this looks up the parameters translation and name, adds it to Params and AllParams, and writes out it's declaration.
        /// </summary>
        /// <param name="parameter"></param>
        void CompileVertexParameter(ParameterSyntax parameter)
        {
            var info = GetModel(parameter).GetSymbolInfo(parameter.Type);

            var symbol = info.Symbol as TypeSymbol;
            if (symbol != null)
            {
                var translation_info = TranslationLookup.RecursiveLookup(symbol);
                if (translation_info.Translation != null)
                {
                    if (translation_info.Translation == "sampler" || translation_info.Translation == "sampler2D")
                    {
                        Write("ERROR(Samplers not suported in vertex shaders : {0})", parameter);
                    }
                    else
                    {
                        string type = translation_info.Translation;
                        string name = parameter.Identifier.ValueText;
                        string mapped_name = VertexShaderParameterPrefix + name;

                        Write(type);
                        Write(" ");
                        Write(mapped_name);
                        Write(";");

                        var param = new Param(parameter.Type.ToString(), type, name, mapped_name, Param.ParamType.VertexParam);
                        Params.Add(param);
                        AllParams.Add(param);
                    }
                }
            }
        }

        /// <summary>
        /// Given a parameter (assumed to be to a fragment shader), this looks up the parameters translation and name, adds it to AllParams. Also, if it's not a specialization, it adds it to Params and writes out it's declaration.
        /// </summary>
        /// <param name="parameter"></param>
        /// <param name="specialized"></param>
        void CompileFragmentParameter(ParameterSyntax parameter, bool specialized)
        {
            var info = GetModel(parameter).GetSymbolInfo(parameter.Type);

            var symbol = info.Symbol as TypeSymbol;
            if (symbol != null)
            {
                var translation_info = TranslationLookup.RecursiveLookup(symbol);
                if (translation_info.Translation != null)
                {
                    if (translation_info.Translation == "sampler" || translation_info.Translation == "sampler2D")
                    {
                        CompileSamplerParameter(parameter, symbol);
                    }
                    else
                    {
                        string type = translation_info.Translation;
                        string name = parameter.Identifier.ValueText;
                        string mapped_name = FragmentShaderParameterPrefix + name;

                        if (!specialized)
                        {
                            Write(type);
                            Write(" ");
                            Write(mapped_name);
                            Write(";");
                        }

                        var param = new Param(parameter.Type.ToString(), type, name, mapped_name, Param.ParamType.FragmentParam);
                        if (!specialized) Params.Add(param);
                        AllParams.Add(param);
                    }
                }
            }
        }

        /// <summary>
        /// All sampler types must derive directly or indirectly from the generic SamplerBase type.
        /// This method takes a sampler type and returns the realized SamplerBase type it derives from.
        /// </summary>
        /// <param name="sampler">The sampler type</param>
        /// <returns></returns>
        NamedTypeSymbol GetSamplerBase(TypeSymbol sampler)
        {
            if (sampler.HasAttribute("SamplerBase")) return sampler as NamedTypeSymbol;
            if (sampler.BaseType != null) return GetSamplerBase(sampler.BaseType);
            return null;
        }

        string TypeToFilter(TypeSymbol type)
        {
            switch (type.Name)
            {
                case "Linear": return "GL_LINEAR";
                case "Point": return "GL_LINEAR_MIPMAP_LINEAR";

                default: return "GL_LINEAR_MIPMAP_LINEAR";
            }
        }

        string TypeToAddress(TypeSymbol type)
        {
            switch (type.Name)
            {
                case "Wrap": return "GL_REPEAT";
                case "Clamp": return "GL_CLAMP_TO_EDGE";

                default: return "GL_CLAMP_TO_EDGE";
            }
        }

        /// <summary>
        /// Given a symbol which corresponds to a forgein sampler parameter, this generates the parameter declaration pointing to shaded memory addresses.
        /// </summary>
        /// <param name="symbol"></param>
        void CompileSamplerParameter_Foreign(Symbol symbol)
        {
            SamplerNumber++;

            string name = symbol.Name;
            string mapped_name = FragmentShaderParameterPrefix + name;

            var type = GetType(symbol);
            var sampler_base = GetSamplerBase(type);
            string address_u = TypeToAddress(sampler_base.TypeArguments[1]);
            string address_v = TypeToAddress(sampler_base.TypeArguments[2]);
            string min_filter = TypeToFilter(sampler_base.TypeArguments[3]);
            string mag_filter = TypeToFilter(sampler_base.TypeArguments[4]);
            string mip_filter = TypeToFilter(sampler_base.TypeArguments[5]);

            Write(SamplerTemplate, Tab, SamplerNumber, mapped_name, address_u, address_v, min_filter, mag_filter, mip_filter);

            var param = new Param("Texture2D", "shader", name, mapped_name, Param.ParamType.FragmentParam);
            param.Foreign = true;
            Params.Add(param);
            AllParams.Add(param);
        }

        /// <summary>
        /// This compiles a declaration for a sampler parameter which is not explicitly forgeign. In practice it doesn't matter, because the declaration points at shared memory addresses.
        /// </summary>
        /// <param name="parameter"></param>
        /// <param name="symbol"></param>
        void CompileSamplerParameter(ParameterSyntax parameter, TypeSymbol symbol)
        {
            SamplerNumber++;

            string name = parameter.Identifier.ValueText;
            string mapped_name = FragmentShaderParameterPrefix + name;

            var sampler_base = GetSamplerBase(symbol);
            string address_u = TypeToAddress(sampler_base.TypeArguments[1]);
            string address_v = TypeToAddress(sampler_base.TypeArguments[2]);
            string min_filter = TypeToFilter(sampler_base.TypeArguments[3]);
            string mag_filter = TypeToFilter(sampler_base.TypeArguments[4]);
            string mip_filter = TypeToFilter(sampler_base.TypeArguments[5]);
            
            Write(SamplerTemplate, Tab, SamplerNumber, mapped_name, address_u, address_v, min_filter, mag_filter, mip_filter);

            var param = new Param("Texture2D", "shader", name, mapped_name, Param.ParamType.FragmentParam);
            Params.Add(param);
            AllParams.Add(param);
        }

        // TODO: deal with this note
        /* NOTE:
         * There seems to be two types of shaders we care about here: vertex shaders and fragment shaders.
         * The way things are set up, we are already creating both types, but we are wrapping them up in a .fx file.
         * ".fx" files are a Hlsl thing, so we will need to figure out how to put together and compile GLSL files.
         */

        // NOTE: this may have problems, noteably: all TexParameter's are "i", I picked stuff at random for glTexImage2D, {2}_Texture has to be the right type, and I'm not sure on the "layout" syntax
const string SamplerTemplate =
@"// Texture Sampler for {2}, using register location {1}
vec2 {2}_" + Sampler.SizeSuffix + @";
vec2 {2}_" + Sampler.DxDySuffix + @";
int {2}_width = {2}_" + Sampler.SizeSuffix + @"[0];
int {2}_height = {2}_" + Sampler.SizeSuffix + @"[1];

glGenTextures(1, &{2}_Texture);
glBindTexture(GL_TEXTURE_2D, {2}_Texture);
glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIP_FILTER, {7}); 
glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, {6});
glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, {5}); 
glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, {3});
glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, {4});
layout (binding = {1}) sampler2D {2}_Texture;
glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, {2}_width, {2}_height, 0, GL_BGRA, GL_UNSIGNED_BYTE, &{2}_Texture);";

const string ReferencedMethodsPreamble =
@"// The following methods are included because they are referenced by the fragment shader.";

const string ReferencedForeignVarsPreamble =
@"// The following variables are included because they are referenced but are not function parameters. Their values will be set at call time.";

const string VertexMethodParameters =
@"// The following are variables used by the vertex shader (vertex parameters).";

const string FragmentMethodParameters =
@"// The following are variables used by the fragment shader (fragment parameters).";

const string VertexShaderBegin =
@"// Compiled vertex shader

attribute vec2 inPos;
attribute vec2 inTexCoords;
attribute vec4 inColor;
varying VertexToPixel CarryOver;

void main ()
{{";

const string VertexShaderEnd =
@"
}}";

const string FragmentShaderBegin =
@"// Compiled fragment shader

varying VertexToPixel CarryOver;
#define psin CarryOver;

void main()
{{
{0}PixelToFrame __FinalOutput = (PixelToFrame)0;";

const string FragmentShaderEnd =
@"}}";

        // TODO: What is this "POSITION0" stuff? we might have to scrap this... Not sure if this syntax is valid for Glsl
const string FileBegin =
@"// This file was auto-generated by FragSharp. It will be regenerated on the next compilation.
// Manual changes made will not persist and may cause incorrect behavior between compilations.

#define PIXEL_SHADER ps_3_0
#define VERTEX_SHADER vs_3_0

// Vertex shader data structure definition
struct VertexToPixel
{{
{0}vec4 Position;
{0}vec4 Color;
{0}vec2 TexCoords;
{0}vec2 Position2D;
}};

// Fragment shader data structure definition
struct PixelToFrame
{{
{0}vec4 Color;
}};";

public const string BoilerFileBegin =
@"// This file was auto-generated by FragSharp. It will be regenerated on the next compilation.
// Manual changes made will not persist and may cause incorrect behavior between compilations.

using System;
using System.Collections.Generic;

using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Content;
using Microsoft.Xna.Framework.Graphics;
using Point = FragSharpFramework.Point;

using FragSharpFramework;";

public const string BoilerBeginInitializer =
@"namespace FragSharpFramework
{{
{0}public class FragSharp
{0}{{
{0}{0}public static ContentManager Content;
{0}{0}public static GraphicsDevice GraphicsDevice;
{0}{0}public static void Initialize(ContentManager Content, GraphicsDevice GraphicsDevice)
{0}{0}{{
{0}{0}{0}FragSharp.Content = Content;
{0}{0}{0}FragSharp.GraphicsDevice = GraphicsDevice;";

public const string BoilerEndInitializer =
@"{0}{0}}}
{0}}}
}}";
    }
}
