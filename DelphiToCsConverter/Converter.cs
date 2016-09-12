using Microsoft.CSharp;
using System;
using System.CodeDom;
using System.CodeDom.Compiler;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using DGrok.Framework;
using DGrok.DelphiNodes;

namespace DelphiToCsConverter
{
    public class Converter
    {
        public string Convert(string delphiCode)
        {
            UnitNode unitNode = CreateAstNodeTree(delphiCode);

            CodeCompileUnit compileUnit = ConvertToCodeDom(unitNode);

            return GenerateCSharp(compileUnit);
        }

        private static string GenerateCSharp(CodeCompileUnit compileUnit)
        {
            using (StringWriter sw = new StringWriter())
            {
                IndentedTextWriter tw = new IndentedTextWriter(sw, "    ");
                CSharpCodeProvider provider = new CSharpCodeProvider();

                provider.GenerateCodeFromCompileUnit(
                    compileUnit,
                    tw,
                    GetCodeGenerationOptions());

                tw.Close();

                return sw.ToString();
            }
        }

        private static CodeCompileUnit ConvertToCodeDom(UnitNode unitNode)
        {
            CodeCompileUnit compileUnit = new CodeCompileUnit();
            CodeNamespace defaultNamespace = new CodeNamespace(unitNode.UnitNameNode.Text);
            //defaultNamespace.Imports.Add(new CodeNamespaceImport("System"));

            compileUnit.Namespaces.Add(defaultNamespace);

            foreach (TypeSectionNode typeSectionNode in unitNode.InterfaceSectionNode.ContentListNode.Items.Where(o => o is TypeSectionNode))
            {

                foreach (AstNode node in typeSectionNode.TypeListNode.Items)
                {
                    if (node is TypeForwardDeclarationNode)
                    {
                        var typeForwardDeclarationNode = node as TypeForwardDeclarationNode;

                        if (typeForwardDeclarationNode.TypeNode.Text == "class")
                        {
                            CodeTypeDeclaration nameClass = new CodeTypeDeclaration(typeForwardDeclarationNode.NameNode.Text);
                            defaultNamespace.Types.Add(nameClass);
                        }
                    }
                    else if (node is TypeDeclNode)
                    {
                        var typeDeclNode = node as TypeDeclNode;

                        if (typeDeclNode.TypeNode is ClassTypeNode)
                        {
                            CodeTypeDeclaration nameClass = new CodeTypeDeclaration(typeDeclNode.NameNode.Text);
                            ConvertClassMembers(nameClass, typeDeclNode.TypeNode as ClassTypeNode);
                            defaultNamespace.Types.Add(nameClass);
                        }
                    }
                }
            }

            return compileUnit;
        }

        private static void ConvertClassMembers(CodeTypeDeclaration nameClass, ClassTypeNode classTypeNode)
        {
            foreach (var visibilityItem in classTypeNode.ContentListNode.Items)
            {
                foreach (var child in visibilityItem.ContentListNode.Items)
                {
                    ConvertMethod(nameClass, (MethodHeadingNode)child);
                }
            }
        }

        private static void ConvertMethod(CodeTypeDeclaration nameClass, MethodHeadingNode child)
        {
            CodeMemberMethod method1 = new CodeMemberMethod();
            method1.Name = child.NameNode.ToCode();
            //method1.ReturnType = new CodeTypeReference("System.String");
            method1.Parameters.Add(new CodeParameterDeclarationExpression(new CodeTypeReference("System.Object"), "Sender"));

            nameClass.Members.Add(method1);
        }

        private static UnitNode CreateAstNodeTree(string delphiCode)
        {
#if NEVER
                // Parse without filtering out comment tokens...
                Lexer lexer = new Lexer(delphiCode, "input");
                Parser parser = Parser.FromTokens(lexer.Tokens);
#endif
            Parser parser = Parser.FromText(delphiCode, "input", CompilerDefines.CreateStandard(),
                new MemoryFileLoader());

            AstNode tree = parser.ParseRule(RuleType.Unit);
            //var Text = tree.Inspect();

            UnitNode unitNode = (UnitNode)tree;
            return unitNode;
        }

        private static CodeGeneratorOptions GetCodeGenerationOptions()
        {
            var options = new CodeGeneratorOptions();
            options.BracingStyle = "C";
            return options;
        }
    }
}
