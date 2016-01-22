import bpy

# Interface to the file.  Supports automatic whitespace indenting.
class File:
    def __init__(self, FilePath):
        self.FilePath = FilePath
        self.File = None
        self.__Whitespace = 0

    def Open(self):
        if not self.File:
            self.File = open(self.FilePath, 'w')

    def Close(self):
        self.File.close()
        self.File = None

    def Write(self, String, Indent=True):
        if Indent:
            # Escape any formatting braces
            String = String.replace("{", "{{")
            String = String.replace("}", "}}")
            self.File.write(("{}" + String).format("  " * self.__Whitespace))
        else:
            self.File.write(String)

    def Indent(self, Levels=1):
        self.__Whitespace += Levels

    def Unindent(self, Levels=1):
        self.__Whitespace -= Levels
        if self.__Whitespace < 0:
            self.__Whitespace = 0

# Static utilities
class Util:
    @staticmethod
    def SafeName(Name):
        # Replaces each character in OldSet with NewChar
        def ReplaceSet(String, OldSet, NewChar):
            for OldChar in OldSet:
                String = String.replace(OldChar, NewChar)
            return String

        import string

        NewName = ReplaceSet(Name, string.punctuation + " ", "_")
        if NewName[0].isdigit() or NewName in ["ARRAY", "DWORD", "UCHAR",
            "FLOAT", "ULONGLONG", "BINARY_RESOURCE", "SDWORD", "UNICODE",
            "CHAR", "STRING", "WORD", "CSTRING", "SWORD", "DOUBLE", "TEMPLATE"]:
            NewName = "_" + NewName
        return NewName

    @staticmethod
    def WriteMatrix(File, Matrix):
        File.Write("{:9f},{:9f},{:9f},{:9f},\n".format(Matrix[0][0],
            Matrix[1][0], Matrix[2][0], Matrix[3][0]))
        File.Write("{:9f},{:9f},{:9f},{:9f},\n".format(Matrix[0][1],
            Matrix[1][1], Matrix[2][1], Matrix[3][1]))
        File.Write("{:9f},{:9f},{:9f},{:9f},\n".format(Matrix[0][2],
            Matrix[1][2], Matrix[2][2], Matrix[3][2]))
        File.Write("{:9f},{:9f},{:9f},{:9f};;\n".format(Matrix[0][3],
            Matrix[1][3], Matrix[2][3], Matrix[3][3]))
    
    # Used on lists of blender objects and lists of ExportObjects, both of
    # which have a name field
    @staticmethod
    def SortByNameField(List):
        def SortKey(x):
            return x.name
        
        return sorted(List, key=SortKey)
    
    # Make A compatible with B
    @staticmethod
    def CompatibleQuaternion(A, B):
        if (A.normalized().conjugated() * B.normalized()).angle > pi:
            return -A
        else:
            return A

# This class wraps a Blender object and writes its data to the file
class ExportObject: # Base class, do not use
    def __init__(self, Config, Exporter, BlenderObject):
        self.Config = Config
        self.Exporter = Exporter
        self.BlenderObject = BlenderObject

        self.name = self.BlenderObject.name # Simple alias
        self.SafeName = Util.SafeName(self.BlenderObject.name)
        self.Children = []

    def __repr__(self):
        return "[ExportObject: {}]".format(self.BlenderObject.name)

    # "Public" Interface

    def Write(self):
        self.Exporter.Log("Opening frame for {}".format(self))
        self._OpenFrame()

        self.Exporter.Log("Writing children of {}".format(self))
        self._WriteChildren()

        self._CloseFrame()
        self.Exporter.Log("Closed frame of {}".format(self))

    # "Protected" Interface

    def _OpenFrame(self):
        self.Exporter.File.Write("Frame {} {{\n".format(self.SafeName))
        self.Exporter.File.Indent()

        self.Exporter.File.Write("FrameTransformMatrix {\n")
        self.Exporter.File.Indent()
        Util.WriteMatrix(self.Exporter.File, self.BlenderObject.matrix_local)
        self.Exporter.File.Unindent()
        self.Exporter.File.Write("}\n")

    def _CloseFrame(self):
        self.Exporter.File.Unindent()
        self.Exporter.File.Write("}} // End of {}\n".format(self.SafeName))

    def _WriteChildren(self):
        for Child in Util.SortByNameField(self.Children):
            Child.Write()
            
class MeshExportObject(ExportObject):
    def __init__(self, Config, Exporter, BlenderObject):
        ExportObject.__init__(self, Config, Exporter, BlenderObject)

    def __repr__(self):
        return "[MeshExportObject: {}]".format(self.name)

    # "Public" Interface

    def Write(self):
        self.Exporter.Log("Opening frame for {}".format(self))
        self._OpenFrame()

        if self.Config.ExportMeshes:
            self.Exporter.Log("Generating mesh for export...")
            # Generate the export mesh
            Mesh = None
            if self.Config.ApplyModifiers:
                # Certain modifiers shouldn't be applied in some cases
                # Deactivate them until after mesh generation is complete
                
                DeactivatedModifierList = []
                
                # If we're exporting armature data, we shouldn't apply
                # armature modifiers to the mesh
                if self.Config.ExportSkinWeights:
                    DeactivatedModifierList = [Modifier
                        for Modifier in self.BlenderObject.modifiers
                        if Modifier.type == 'ARMATURE' and \
                        Modifier.show_viewport]
                
                for Modifier in DeactivatedModifierList:
                    Modifier.show_viewport = False
                        
                Mesh = self.BlenderObject.to_mesh(self.Exporter.context.scene,
                    True, 'PREVIEW')
                
                # Restore the deactivated modifiers
                for Modifier in DeactivatedModifierList:
                    Modifier.show_viewport = True   
            else:
                Mesh = self.BlenderObject.to_mesh(self.Exporter.context.scene,
                    False, 'PREVIEW')
            self.Exporter.Log("Done")
                    
            self.__WriteMesh(Mesh)

            # Cleanup
            bpy.data.meshes.remove(Mesh)

        self.Exporter.Log("Writing children of {}".format(self))
        self._WriteChildren()

        self._CloseFrame()
        self.Exporter.Log("Closed frame of {}".format(self))

    # "Protected"
    
    # This class provides a general system for indexing a mesh, depending on
    # exporter needs.  For instance, some options require us to duplicate each
    # vertex of each face, some can reuse vertex data.  For those we'd use
    # _UnrolledFacesMeshEnumerator and _OneToOneMeshEnumerator respectively.
    class _MeshEnumerator:
        def __init__(self, Mesh):
            self.Mesh = Mesh
            
            # self.vertices and self.PolygonVertexIndices relate to the
            # original mesh in the following way:
            
            # Mesh.vertices[Mesh.polygons[x].vertices[y]] == 
            # self.vertices[self.PolygonVertexIndices[x][y]]
            
            self.vertices = None 
            self.PolygonVertexIndices = None
    
    # Represents the mesh as it is inside Blender
    class _OneToOneMeshEnumerator(_MeshEnumerator):
        def __init__(self, Mesh):
            MeshExportObject._MeshEnumerator.__init__(self, Mesh)
            
            self.vertices = Mesh.vertices
            
            self.PolygonVertexIndices = tuple(tuple(Polygon.vertices)
                for Polygon in Mesh.polygons)

    # Duplicates each vertex for each face
    class _UnrolledFacesMeshEnumerator(_MeshEnumerator):
        def __init__(self, Mesh):
            MeshExportObject._MeshEnumerator.__init__(self, Mesh)
            
            self.vertices = tuple()
            for Polygon in Mesh.polygons:
                self.vertices += tuple(Mesh.vertices[VertexIndex]
                    for VertexIndex in Polygon.vertices)
            
            self.PolygonVertexIndices = []
            Index = 0
            for Polygon in Mesh.polygons:
                self.PolygonVertexIndices.append(tuple(range(Index, 
                    Index + len(Polygon.vertices))))
                Index += len(Polygon.vertices)
            
    # "Private" Methods

    def __WriteMesh(self, Mesh):
        self.Exporter.Log("Writing mesh vertices...")
        self.Exporter.File.Write("Mesh {{ // {} mesh\n".format(self.SafeName))
        self.Exporter.File.Indent()
        
        # Create the mesh enumerator based on options
        MeshEnumerator = None
        if (self.Config.ExportUVCoordinates and Mesh.uv_textures) or \
            (self.Config.ExportVertexColors and Mesh.vertex_colors) or \
            (self.Config.ExportSkinWeights):
            MeshEnumerator = MeshExportObject._UnrolledFacesMeshEnumerator(Mesh)
        else:
            MeshEnumerator = MeshExportObject._OneToOneMeshEnumerator(Mesh)
        
        # Write vertex positions
        VertexCount = len(MeshEnumerator.vertices)
        self.Exporter.File.Write("{};\n".format(VertexCount))
        for Index, Vertex in enumerate(MeshEnumerator.vertices):
            Position = Vertex.co
            self.Exporter.File.Write("{:9f};{:9f};{:9f};".format(
                        Position[0], Position[1], Position[2]))
            
            if Index == VertexCount - 1:
                self.Exporter.File.Write(";\n", Indent=False)
            else:
                self.Exporter.File.Write(",\n", Indent=False)
        
        # Write face definitions
        PolygonCount = len(MeshEnumerator.PolygonVertexIndices)
        self.Exporter.File.Write("{};\n".format(PolygonCount))
        for Index, PolygonVertexIndices in \
            enumerate(MeshEnumerator.PolygonVertexIndices):
            
            self.Exporter.File.Write("{};".format(len(PolygonVertexIndices)))
            
            if self.Config.CoordinateSystem == 'LEFT_HANDED':
                PolygonVertexIndices = PolygonVertexIndices[::-1]
            
            for VertexCountIndex, VertexIndex in \
                enumerate(PolygonVertexIndices):

                if VertexCountIndex == len(PolygonVertexIndices) - 1:
                    self.Exporter.File.Write("{};".format(VertexIndex),
                        Indent=False)
                else:
                    self.Exporter.File.Write("{},".format(VertexIndex),
                        Indent=False)
            
            if Index == PolygonCount - 1:
                self.Exporter.File.Write(";\n", Indent=False)
            else:
                self.Exporter.File.Write(",\n", Indent=False)
        self.Exporter.Log("Done")
        
        # Write the other mesh components
            
        if self.Config.ExportNormals:
            self.Exporter.Log("Writing mesh normals...")
            self.__WriteMeshNormals(Mesh)
            self.Exporter.Log("Done")
            
        if self.Config.ExportUVCoordinates:
            self.Exporter.Log("Writing mesh UV coordinates...")
            self.__WriteMeshUVCoordinates(Mesh)
            self.Exporter.Log("Done")

        if self.Config.ExportMaterials:
            self.Exporter.Log("Writing mesh materials...")
            if self.Config.ExportActiveImageMaterials:
                self.Exporter.Log("Referencing active images instead of "\
                    "material image textures.")
                self.__WriteMeshActiveImageMaterials(Mesh)
            else:
                self.__WriteMeshMaterials(Mesh)
            self.Exporter.Log("Done")
        
        if self.Config.ExportVertexColors:
            self.Exporter.Log("Writing mesh vertex colors...")
            self.__WriteMeshVertexColors(Mesh, MeshEnumerator=MeshEnumerator)
            self.Exporter.Log("Done")
        
        if self.Config.ExportSkinWeights:
            self.Exporter.Log("Writing mesh skin weights...")
            self.__WriteMeshSkinWeights(Mesh, MeshEnumerator=MeshEnumerator)
            self.Exporter.Log("Done")

        self.Exporter.File.Unindent()
        self.Exporter.File.Write("}} // End of {} mesh\n".format(self.SafeName))

    def __WriteMeshNormals(self, Mesh, MeshEnumerator=None):
        # Since mesh normals only need their face counts and vertices per face
        # to match up with the other mesh data, we can optimize export with
        # this enumerator.  Exports each vertex's normal when a face is shaded
        # smooth, and exports the face normal only once when a face is shaded
        # flat.
        class _NormalsMeshEnumerator(MeshExportObject._MeshEnumerator):
            def __init__(self, Mesh):
                MeshExportObject._MeshEnumerator(Mesh)
                
                self.vertices = []
                self.PolygonVertexIndices = []
                
                Index = 0
                for Polygon in Mesh.polygons:
                    if not Polygon.use_smooth:
                        self.vertices.append(Polygon)
                        self.PolygonVertexIndices.append(
                            tuple(len(Polygon.vertices) * [Index]))
                        Index += 1
                    else:
                        for VertexIndex in Polygon.vertices:
                            self.vertices.append(Mesh.vertices[VertexIndex])
                        self.PolygonVertexIndices.append(
                            tuple(range(Index, Index + len(Polygon.vertices))))
                        Index += len(Polygon.vertices)            
        
        if MeshEnumerator is None:
            MeshEnumerator = _NormalsMeshEnumerator(Mesh)
        
        self.Exporter.File.Write("MeshNormals {{ // {} normals\n".format(
            self.SafeName))
        self.Exporter.File.Indent()
        
        NormalCount = len(MeshEnumerator.vertices)
        self.Exporter.File.Write("{};\n".format(NormalCount))
        
        # Write mesh normals.
        for Index, Vertex in enumerate(MeshEnumerator.vertices):
            Normal = Vertex.normal
            if self.Config.FlipNormals:
                Normal = -1.0 * Vertex.normal
            
            self.Exporter.File.Write("{:9f};{:9f};{:9f};".format(Normal[0],
                Normal[1], Normal[2]))
            
            if Index == NormalCount - 1:
                self.Exporter.File.Write(";\n", Indent=False)
            else:
                self.Exporter.File.Write(",\n", Indent=False)
        
        # Write face definitions.
        FaceCount = len(MeshEnumerator.PolygonVertexIndices)
        self.Exporter.File.Write("{};\n".format(FaceCount))
        for Index, Polygon in enumerate(MeshEnumerator.PolygonVertexIndices):
            self.Exporter.File.Write("{};".format(len(Polygon)))
            
            if self.Config.CoordinateSystem == 'LEFT_HANDED':
                Polygon = Polygon[::-1]
            
            for VertexCountIndex, VertexIndex in enumerate(Polygon):
                if VertexCountIndex == len(Polygon) - 1:
                    self.Exporter.File.Write("{};".format(VertexIndex),
                        Indent=False)
                else:
                    self.Exporter.File.Write("{},".format(VertexIndex),
                        Indent=False)
            
            if Index == FaceCount - 1:
                self.Exporter.File.Write(";\n", Indent=False)
            else:
                self.Exporter.File.Write(",\n", Indent=False)

        self.Exporter.File.Unindent()
        self.Exporter.File.Write("}} // End of {} normals\n".format(
            self.SafeName))
     
    def __WriteMeshUVCoordinates(self, Mesh):
        if not Mesh.uv_textures:
            return
        
        self.Exporter.File.Write("MeshTextureCoords {{ // {} UV coordinates\n" \
            .format(self.SafeName))
        self.Exporter.File.Indent()
        
        UVCoordinates = Mesh.uv_layers.active.data
        
        VertexCount = 0
        for Polygon in Mesh.polygons:
            VertexCount += len(Polygon.vertices)
        
        # Gather and write UV coordinates
        Index = 0
        self.Exporter.File.Write("{};\n".format(VertexCount))
        for Polygon in Mesh.polygons:
            Vertices = []
            for Vertex in [UVCoordinates[Vertex] for Vertex in
                Polygon.loop_indices]:
                Vertices.append(tuple(Vertex.uv))
            for Vertex in Vertices:
                self.Exporter.File.Write("{:9f};{:9f};".format(Vertex[0],
                    1.0 - Vertex[1]))
                Index += 1
                if Index == VertexCount:
                    self.Exporter.File.Write(";\n", Indent=False)
                else:
                    self.Exporter.File.Write(",\n", Indent=False)
                    
        self.Exporter.File.Unindent()
        self.Exporter.File.Write("}} // End of {} UV coordinates\n".format(
            self.SafeName))

    def __WriteMeshMaterials(self, Mesh):
        def WriteMaterial(Exporter, Material):
            def GetMaterialTextureFileName(Material):
                if Material:
                    # Create a list of Textures that have type 'IMAGE'
                    ImageTextures = [Material.texture_slots[TextureSlot].texture
                        for TextureSlot in Material.texture_slots.keys()
                        if Material.texture_slots[TextureSlot].texture.type ==
                        'IMAGE']
                    # Refine to only image file names if applicable
                    ImageFiles = [bpy.path.basename(Texture.image.filepath)
                        for Texture in ImageTextures
                        if getattr(Texture.image, "source", "") == 'FILE']
                    if ImageFiles:
                        return ImageFiles[0]
                return None
            
            Exporter.File.Write("Material {} {{\n".format(
                Util.SafeName(Material.name)))
            Exporter.File.Indent()
            
            Diffuse = list(Vector(Material.diffuse_color) *
                Material.diffuse_intensity)
            Diffuse.append(Material.alpha)
            # Map Blender's range of 1 - 511 to 0 - 1000
            Specularity = 1000 * (Material.specular_hardness - 1.0) / 510.0
            Specular = list(Vector(Material.specular_color) *
                Material.specular_intensity)
            
            Exporter.File.Write("{:9f};{:9f};{:9f};{:9f};;\n".format(Diffuse[0],
                Diffuse[1], Diffuse[2], Diffuse[3]))
            Exporter.File.Write(" {:9f};\n".format(Specularity))
            Exporter.File.Write("{:9f};{:9f};{:9f};;\n".format(Specular[0],
                Specular[1], Specular[2]))
            Exporter.File.Write(" 0.000000; 0.000000; 0.000000;;\n")
            
            TextureFileName = GetMaterialTextureFileName(Material)
            if TextureFileName:
                Exporter.File.Write("TextureFilename {{\"{}\";}}\n".format(
                    TextureFileName))
            
            Exporter.File.Unindent()
            Exporter.File.Write("}\n");
        
        Materials = Mesh.materials
        # Do not write materials if there are none
        if not Materials.keys():
            return
        
        self.Exporter.File.Write("MeshMaterialList {{ // {} material list\n".
            format(self.SafeName))
        self.Exporter.File.Indent()
        
        self.Exporter.File.Write("{};\n".format(len(Materials)))
        self.Exporter.File.Write("{};\n".format(len(Mesh.polygons)))
        # Write a material index for each face
        for Index, Polygon in enumerate(Mesh.polygons):
            self.Exporter.File.Write("{}".format(Polygon.material_index))
            if Index == len(Mesh.polygons) - 1:
                self.Exporter.File.Write(";;\n", Indent=False)
            else:
                self.Exporter.File.Write(",\n", Indent=False)
        
        for Material in Materials:
            WriteMaterial(self.Exporter, Material)
        
        self.Exporter.File.Unindent()
        self.Exporter.File.Write("}} // End of {} material list\n".format(
            self.SafeName))
    
    def __WriteMeshActiveImageMaterials(self, Mesh):
        def WriteMaterial(Exporter, MaterialKey):
            #Unpack key
            Material, Image = MaterialKey
            
            Exporter.File.Write("Material {} {{\n".format(
                Util.SafeName(Material.name)))
            Exporter.File.Indent()
            
            Diffuse = list(Vector(Material.diffuse_color) *
                Material.diffuse_intensity)
            Diffuse.append(Material.alpha)
            # Map Blender's range of 1 - 511 to 0 - 1000
            Specularity = 1000 * (Material.specular_hardness - 1.0) / 510.0
            Specular = list(Vector(Material.specular_color) *
                Material.specular_intensity)
            
            Exporter.File.Write("{:9f};{:9f};{:9f};{:9f};;\n".format(Diffuse[0],
                Diffuse[1], Diffuse[2], Diffuse[3]))
            Exporter.File.Write(" {:9f};\n".format(Specularity))
            Exporter.File.Write("{:9f};{:9f};{:9f};;\n".format(Specular[0],
                Specular[1], Specular[2]))
            Exporter.File.Write(" 0.000000; 0.000000; 0.000000;;\n")
            
            if Image is not None:
                Exporter.File.Write("TextureFilename {{\"{}\";}}\n".format(
                    bpy.path.basename(Image.filepath)))
            
            self.Exporter.File.Unindent()
            self.Exporter.File.Write("}\n")
        
        def GetMaterialKey(Material, UVTexture, Index):
            Image = None
            if UVTexture != None and UVTexture.data[Index].image != None:
                Image = UVTexture.data[Index].image if \
                    UVTexture.data[Index].image.source == 'FILE' else None
            
            return (Material, Image)
        
        Materials = Mesh.materials
        # Do not write materials if there are none
        if not Materials.keys():
            return
        
        self.Exporter.File.Write("MeshMaterialList {{ // {} material list\n".
            format(self.SafeName))
        self.Exporter.File.Indent()
        
        from array import array
        MaterialIndices = array("I", [0]) * len(Mesh.polygons) # Fast allocate
        MaterialIndexMap = {}
        
        for Index, Polygon in enumerate(Mesh.polygons):
            MaterialKey = GetMaterialKey(Materials[Polygon.material_index],
                Mesh.uv_textures.active, Index)
            
            if MaterialKey in MaterialIndexMap:
                MaterialIndices[Index] = MaterialIndexMap[MaterialKey]
            else:
                MaterialIndex = len(MaterialIndexMap)
                MaterialIndexMap[MaterialKey] = MaterialIndex
                MaterialIndices[Index] = MaterialIndex
        
        self.Exporter.File.Write("{};\n".format(len(MaterialIndexMap)))
        self.Exporter.File.Write("{};\n".format(len(Mesh.polygons)))
        
        for Index in range(len(Mesh.polygons)):
            self.Exporter.File.Write("{}".format(MaterialIndices[Index]))
            if Index == len(Mesh.polygons) - 1:
                self.Exporter.File.Write(";;\n", Indent=False)
            else:
                self.Exporter.File.Write(",\n", Indent=False)
        
        for Material in MaterialIndexMap.keys():
            WriteMaterial(self.Exporter, Material)
        
        self.Exporter.File.Unindent()
        self.Exporter.File.Write("}} // End of {} material list\n".format(
            self.SafeName))
    
    def __WriteMeshVertexColors(self, Mesh, MeshEnumerator=None):
        # If there are no vertex colors, don't write anything
        if len(Mesh.vertex_colors) == 0:
            return
        
        # Blender stores vertex color information per vertex per face, so we
        # need to pass in an _UnrolledFacesMeshEnumerator.  Otherwise,
        if MeshEnumerator is None:
            MeshEnumerator = _UnrolledFacesMeshEnumerator(Mesh)
        
        # Gather the colors of each vertex
        VertexColorLayer = Mesh.vertex_colors.active
        VertexColors = [VertexColorLayer.data[Index].color for Index in
            range(0,len(MeshEnumerator.vertices))]
        VertexColorCount = len(VertexColors)
        
        self.Exporter.File.Write("MeshVertexColors {{ // {} vertex colors\n" \
            .format(self.SafeName))
        self.Exporter.File.Indent()
        self.Exporter.File.Write("{};\n".format(VertexColorCount))
        
        # Write the vertex colors for each vertex index.
        for Index, Color in enumerate(VertexColors):
            self.Exporter.File.Write("{};{:9f};{:9f};{:9f};{:9f};;".format(
                Index, Color[0], Color[1], Color[2], 1.0))
            
            if Index == VertexColorCount - 1:
                self.Exporter.File.Write(";\n", Indent=False)
            else:
                self.Exporter.File.Write(",\n", Indent=False)
        
        self.Exporter.File.Unindent()
        self.Exporter.File.Write("}} // End of {} vertex colors\n".format(
            self.SafeName))
    
    def __WriteMeshSkinWeights(self, Mesh, MeshEnumerator=None):
        # This contains vertex indices and weights for the vertices that belong
        # to this bone's group.  Also calculates the bone skin matrix.
        class _BoneVertexGroup:
                def __init__(self, BlenderObject, ArmatureObject, BoneName):
                    self.BoneName = BoneName
                    self.SafeName = Util.SafeName(ArmatureObject.name) + "_" + \
                        Util.SafeName(BoneName)
                    
                    self.Indices = []
                    self.Weights = []
                    
                    # BoneMatrix transforms mesh vertices into the
                    # space of the bone.
                    # Here are the final transformations in order:
                    #  - Object Space to World Space
                    #  - World Space to Armature Space
                    #  - Armature Space to Bone Space
                    # This way, when BoneMatrix is transformed by the bone's
                    # Frame matrix, the vertices will be in their final world
                    # position.
                    
                    self.BoneMatrix = ArmatureObject.data.bones[BoneName] \
                        .matrix_local.inverted()
                    self.BoneMatrix *= ArmatureObject.matrix_world.inverted()
                    self.BoneMatrix *= BlenderObject.matrix_world
                
                def AddVertex(self, Index, Weight):
                    self.Indices.append(Index)
                    self.Weights.append(Weight)
        
        # Skin weights work well with vertex reuse per face.  Use a
        # _OneToOneMeshEnumerator if possible.
        if MeshEnumerator is None:
            MeshEnumerator = MeshExportObject._OneToOneMeshEnumerator(Mesh)
        
        ArmatureModifierList = [Modifier 
            for Modifier in self.BlenderObject.modifiers
            if Modifier.type == 'ARMATURE' and Modifier.show_viewport]
        
        if not ArmatureModifierList:
            return
        
        # Although multiple armature objects are gathered, support for
        # multiple armatures per mesh is not complete
        ArmatureObjects = [Modifier.object for Modifier in ArmatureModifierList]
        
        for ArmatureObject in ArmatureObjects:
            # Determine the names of the bone vertex groups
            PoseBoneNames = [Bone.name for Bone in ArmatureObject.pose.bones]
            VertexGroupNames = [Group.name for Group
                in self.BlenderObject.vertex_groups]
            UsedBoneNames = set(PoseBoneNames).intersection(VertexGroupNames)
            
            # Create a _BoneVertexGroup for each group name
            BoneVertexGroups = [_BoneVertexGroup(self.BlenderObject,
                ArmatureObject, BoneName) for BoneName in UsedBoneNames]
            
            # Maps Blender's internal group indexing to our _BoneVertexGroups
            GroupIndexToBoneVertexGroups = {Group.index : BoneVertexGroup
                for Group in self.BlenderObject.vertex_groups
                for BoneVertexGroup in BoneVertexGroups
                if Group.name == BoneVertexGroup.BoneName}
            
            MaximumInfluences = 0
            
            for Index, Vertex in enumerate(MeshEnumerator.vertices):
                VertexWeightTotal = 0.0
                VertexInfluences = 0
                
                # Sum up the weights of groups that correspond
                # to armature bones.
                for VertexGroup in Vertex.groups:
                    BoneVertexGroup = GroupIndexToBoneVertexGroups.get(
                        VertexGroup.group)
                    if BoneVertexGroup is not None:
                        VertexWeightTotal += VertexGroup.weight
                        VertexInfluences += 1
                
                if VertexInfluences > MaximumInfluences:
                    MaximumInfluences = VertexInfluences
                
                # Add the vertex to the bone vertex groups it belongs to,
                # normalizing each bone's weight.
                for VertexGroup in Vertex.groups:
                    BoneVertexGroup = GroupIndexToBoneVertexGroups.get(
                        VertexGroup.group)
                    if BoneVertexGroup is not None:
                        Weight = VertexGroup.weight / VertexWeightTotal
                        BoneVertexGroup.AddVertex(Index, Weight)
            
            self.Exporter.File.Write("XSkinMeshHeader {\n")
            self.Exporter.File.Indent()
            self.Exporter.File.Write("{};\n".format(MaximumInfluences))
            self.Exporter.File.Write("{};\n".format(3 * MaximumInfluences))
            self.Exporter.File.Write("{};\n".format(len(BoneVertexGroups)))
            self.Exporter.File.Unindent()
            self.Exporter.File.Write("}\n")
            
            for BoneVertexGroup in BoneVertexGroups:
                self.Exporter.File.Write("SkinWeights {\n")
                self.Exporter.File.Indent()
                self.Exporter.File.Write("\"{}\";\n".format(
                    BoneVertexGroup.SafeName))
                
                GroupVertexCount = len(BoneVertexGroup.Indices)
                self.Exporter.File.Write("{};\n".format(GroupVertexCount))
                
                # Write the indices of the vertices this bone affects.
                for Index, VertexIndex in enumerate(BoneVertexGroup.Indices):
                    self.Exporter.File.Write("{}".format(VertexIndex))
                    
                    if Index == GroupVertexCount - 1:
                        self.Exporter.File.Write(";\n", Indent=False)
                    else:
                        self.Exporter.File.Write(",\n", Indent=False)
                
                # Write the weights of the affected vertices.
                for Index, VertexWeight in enumerate(BoneVertexGroup.Weights):
                    self.Exporter.File.Write("{:9f}".format(VertexWeight))
                    
                    if Index == GroupVertexCount - 1:
                        self.Exporter.File.Write(";\n", Indent=False)
                    else:
                        self.Exporter.File.Write(",\n", Indent=False)
                
                # Write the bone's matrix.
                Util.WriteMatrix(self.Exporter.File, BoneVertexGroup.BoneMatrix)
            
                self.Exporter.File.Unindent()
                self.Exporter.File.Write("}} // End of {} skin weights\n" \
                    .format(BoneVertexGroup.SafeName))

# Armature object implementation of ExportObject            
class ArmatureExportObject(ExportObject):
    def __init__(self, Config, Exporter, BlenderObject):
        ExportObject.__init__(self, Config, Exporter, BlenderObject)

    def __repr__(self):
        return "[ArmatureExportObject: {}]".format(self.name)
    
    # "Public" Interface

    def Write(self):
        self.Exporter.Log("Opening frame for {}".format(self))
        self._OpenFrame()
        
        if self.Config.ExportArmatureBones:
            Armature = self.BlenderObject.data
            RootBones = [Bone for Bone in Armature.bones if Bone.parent is None]
            self.Exporter.Log("Writing frames for armature bones...")
            self.__WriteBones(RootBones)
            self.Exporter.Log("Done")

        self.Exporter.Log("Writing children of {}".format(self))
        self._WriteChildren()

        self._CloseFrame()
        self.Exporter.Log("Closed frame of {}".format(self))
    
    # "Private" Methods
    
    def __WriteBones(self, Bones):
        # Simply export the frames for each bone.  Export in rest position or
        # posed position depending on options.
        for Bone in Bones:
            BoneMatrix = Matrix()
            
            if self.Config.ExportRestBone:
                if Bone.parent:
                    BoneMatrix = Bone.parent.matrix_local.inverted()
                BoneMatrix *= Bone.matrix_local
            else:
                PoseBone = self.BlenderObject.pose.bones[Bone.name]
                if Bone.parent:
                    BoneMatrix = PoseBone.parent.matrix.inverted()
                BoneMatrix *= PoseBone.matrix
            
            BoneSafeName = self.SafeName + "_" + \
                Util.SafeName(Bone.name)
            self.__OpenBoneFrame(BoneSafeName, BoneMatrix)
            
            self.__WriteBoneChildren(Bone)
            
            self.__CloseBoneFrame(BoneSafeName)
            
    
    def __OpenBoneFrame(self, BoneSafeName, BoneMatrix):
        self.Exporter.File.Write("Frame {} {{\n".format(BoneSafeName))
        self.Exporter.File.Indent()

        self.Exporter.File.Write("FrameTransformMatrix {\n")
        self.Exporter.File.Indent()
        Util.WriteMatrix(self.Exporter.File, BoneMatrix)
        self.Exporter.File.Unindent()
        self.Exporter.File.Write("}\n")
    
    def __CloseBoneFrame(self, BoneSafeName):
        self.Exporter.File.Unindent()
        self.Exporter.File.Write("}} // End of {}\n".format(BoneSafeName))
    
    def __WriteBoneChildren(self, Bone):
        self.__WriteBones(Util.SortByNameField(Bone.children))
                     
def save(context, filepath):                  
    class Self:
        def _OpenFrame(self):
            pass
            
        def Log(self, s):
            print(s)
        pass

    class Exporter:
        def _OpenFrame(self):
            pass
            
        def Log(self, s):
            print(s)
            
        def Export(self):
            if self.Config.SelectedOnly:
                ExportList = list(self.context.selected_objects)
            else:
                ExportList = list(self.context.scene.objects)
                
            # ExportMap maps Blender objects to ExportObjects
            ExportMap = {}
            for Object in ExportList:
                print("ExportMap", Object.name, Object.parent, Object.type)
        #        if Object.type == 'EMPTY':
        #            ExportMap[Object] = EmptyExportObject(self.Config, self, Object)
        #        el
                if Object.type == 'MESH':
                    ExportMap[Object] = MeshExportObject(self.Config, self,
                        Object)
                elif Object.type == 'ARMATURE':
                    ExportMap[Object] = ArmatureExportObject(self.Config, self,
                        Object)

            self.RootExportList = [Object for Object in ExportMap.values()
                if Object.BlenderObject.parent not in ExportList]
                
            for Object in ExportMap.values():
                Children = Object.BlenderObject.children
                Object.Children = []
                for Child in Children:
                    if Child in ExportMap:
                        Object.Children.append(ExportMap[Child])

            # Export everything
            self.Log("Opening file...")
            self.File.Open()
            self.Log("Done")

            self.Log("Writing objects...")
            for Object in self.RootExportList:
                print("RootExportList", Object.BlenderObject.name, Object.BlenderObject.parent)
                Object.Write()
            self.Log("Done writing objects")

    self = Self()
    self.context = context
    self.BlenderObject = context
    self.Exporter = Exporter()
    self.Config = Self()
    self.Config.SelectedOnly = False
    self.Config.ExportMeshes = True
    self.Config.ApplyModifiers = False
    self.Config.ExportUVCoordinates = False
    self.Config.ExportArmatureBones = False
    self.Config.ExportVertexColors = False
    self.Config.ExportSkinWeights = True
    self.Config.ExportNormals = False
    self.Config.ExportMaterials = False
    self.Config.CoordinateSystem = 'LEFT_HANDED'
    self.Exporter.Config = self.Config
    self.Exporter.context = self.context
    self.Exporter.File = File(filepath)
    
    self.Exporter.Export()

save(bpy.context, 'd:\\temp\\peasant\\test.x')