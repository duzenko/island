# ##### BEGIN GPL LICENSE BLOCK #####
#
#  This program is free software; you can redistribute it and/or
#  modify it under the terms of the GNU General Public License
#  as published by the Free Software Foundation; either version 2
#  of the License, or (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software Foundation,
#  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
# ##### END GPL LICENSE BLOCK #####

# <pep8 compliant>

import os
import bpy
import math
import mathutils
import bpy_extras.io_utils
import sys

from progress_report import ProgressReport, ProgressReportSubstep

def name_compat(name):
    if name is None:
        return 'None'
    else:
        return name.replace(' ', '_')

def mesh_triangulate(me):
    import bmesh
    bm = bmesh.new()
    bm.from_mesh(me)
    bmesh.ops.triangulate(bm, faces=bm.faces)
    bm.to_mesh(me)
    bm.free()

def write_mtl(scene, filepath, path_mode, copy_set, mtl_dict):
    from mathutils import Color

    world = scene.world
    if world:
        world_amb = world.ambient_color
    else:
        world_amb = Color((0.0, 0.0, 0.0))

    source_dir = os.path.dirname(bpy.data.filepath)
    dest_dir = os.path.dirname(filepath)

    with open(filepath, "w", encoding="utf8", newline="\n") as f:
        fw = f.write

        fw('# Blender MTL File: %r\n' % (os.path.basename(bpy.data.filepath) or "None"))
        fw('# Material Count: %i\n' % len(mtl_dict))

        mtl_dict_values = list(mtl_dict.values())
        mtl_dict_values.sort(key=lambda m: m[0])

        # Write material/image combinations we have used.
        # Using mtl_dict.values() directly gives un-predictable order.
        for mtl_mat_name, mat, face_img in mtl_dict_values:
            # Get the Blender data for the material and the image.
            # Having an image named None will make a bug, dont do it :)

            fw('\nnewmtl %s\n' % mtl_mat_name)  # Define a new material: matname_imgname

            if mat:
                use_mirror = mat.raytrace_mirror.use and mat.raytrace_mirror.reflect_factor != 0.0

                # convert from blenders spec to 0 - 1000 range.
                if mat.specular_shader == 'WARDISO':
                    tspec = (0.4 - mat.specular_slope) / 0.0004
                else:
                    tspec = (mat.specular_hardness - 1) / 0.51
                fw('Ns %.6f\n' % tspec)
                del tspec

                # Ambient
                if use_mirror:
                    fw('Ka %.6f %.6f %.6f\n' % (mat.raytrace_mirror.reflect_factor * mat.mirror_color)[:])
                else:
                    fw('Ka %.6f %.6f %.6f\n' % (mat.ambient, mat.ambient, mat.ambient))  # Do not use world color!
                fw('Kd %.6f %.6f %.6f\n' % (mat.diffuse_intensity * mat.diffuse_color)[:])  # Diffuse
                fw('Ks %.6f %.6f %.6f\n' % (mat.specular_intensity * mat.specular_color)[:])  # Specular
                # Emission, not in original MTL standard but seems pretty common, see T45766.
                # XXX Blender has no color emission, it's using diffuse color instead...
                fw('Ke %.6f %.6f %.6f\n' % (mat.emit * mat.diffuse_color)[:])
                if hasattr(mat, "raytrace_transparency") and hasattr(mat.raytrace_transparency, "ior"):
                    fw('Ni %.6f\n' % mat.raytrace_transparency.ior)  # Refraction index
                else:
                    fw('Ni %.6f\n' % 1.0)
                fw('d %.6f\n' % mat.alpha)  # Alpha (obj uses 'd' for dissolve)

                # See http://en.wikipedia.org/wiki/Wavefront_.obj_file for whole list of values...
                # Note that mapping is rather fuzzy sometimes, trying to do our best here.
                if mat.use_shadeless:
                    fw('illum 0\n')  # ignore lighting
                elif mat.specular_intensity == 0:
                    fw('illum 1\n')  # no specular.
                elif use_mirror:
                    if mat.use_transparency and mat.transparency_method == 'RAYTRACE':
                        if mat.raytrace_mirror.fresnel != 0.0:
                            fw('illum 7\n')  # Reflection, Transparency, Ray trace and Fresnel
                        else:
                            fw('illum 6\n')  # Reflection, Transparency, Ray trace
                    elif mat.raytrace_mirror.fresnel != 0.0:
                        fw('illum 5\n')  # Reflection, Ray trace and Fresnel
                    else:
                        fw('illum 3\n')  # Reflection and Ray trace
                elif mat.use_transparency and mat.transparency_method == 'RAYTRACE':
                    fw('illum 9\n')  # 'Glass' transparency and no Ray trace reflection... fuzzy matching, but...
                else:
                    fw('illum 2\n')  # light normaly

            else:
                # Write a dummy material here?
                fw('Ns 0\n')
                fw('Ka %.6f %.6f %.6f\n' % world_amb[:])  # Ambient, uses mirror color,
                fw('Kd 0.8 0.8 0.8\n')
                fw('Ks 0.8 0.8 0.8\n')
                fw('d 1\n')  # No alpha
                fw('illum 2\n')  # light normaly

            # Write images!
            if face_img:  # We have an image on the face!
                filepath = face_img.filepath
                if filepath:  # may be '' for generated images
                    # write relative image path
                    filepath = bpy_extras.io_utils.path_reference(filepath, source_dir, dest_dir,
                                                                  path_mode, "", copy_set, face_img.library)
                    fw('map_Kd %s\n' % filepath)  # Diffuse mapping image
                    del filepath
                else:
                    # so we write the materials image.
                    face_img = None

            if mat:  # No face image. if we havea material search for MTex image.
                image_map = {}
                # backwards so topmost are highest priority
                for mtex in reversed(mat.texture_slots):
                    if mtex and mtex.texture and mtex.texture.type == 'IMAGE':
                        image = mtex.texture.image
                        if image:
                            # texface overrides others
                            if (mtex.use_map_color_diffuse and (face_img is None) and
                                (mtex.use_map_warp is False) and (mtex.texture_coords != 'REFLECTION')):
                                image_map["map_Kd"] = image
                            if mtex.use_map_ambient:
                                image_map["map_Ka"] = image
                            # this is the Spec intensity channel but Ks stands for specular Color
                            '''
                            if mtex.use_map_specular:
                                image_map["map_Ks"] = image
                            '''
                            if mtex.use_map_color_spec:  # specular color
                                image_map["map_Ks"] = image
                            if mtex.use_map_hardness:  # specular hardness/glossiness
                                image_map["map_Ns"] = image
                            if mtex.use_map_alpha:
                                image_map["map_d"] = image
                            if mtex.use_map_translucency:
                                image_map["map_Tr"] = image
                            if mtex.use_map_normal:
                                image_map["map_Bump"] = image
                            if mtex.use_map_displacement:
                                image_map["disp"] = image
                            if mtex.use_map_color_diffuse and (mtex.texture_coords == 'REFLECTION'):
                                image_map["refl"] = image
                            if mtex.use_map_emit:
                                image_map["map_Ke"] = image

                for key, image in sorted(image_map.items()):
                    filepath = bpy_extras.io_utils.path_reference(image.filepath, source_dir, dest_dir,
                                                                  path_mode, "", copy_set, image.library)
                    fw('%s %s\n' % (key, repr(filepath)[1:-1]))


def write_obj_matrix(context, scene, filepath, objects):
 
    scene_frames = range(scene.frame_start, scene.frame_end + 1)  # Up to and including the end frame.

    # Loop through all frames in the scene and export.
    with open(filepath, "w", encoding="utf8", newline="\n") as f:
        fw = f.write
        for frame in scene_frames:
#            print('frame', frame)
            scene.frame_set(frame, 0.0)
            for i, ob_main in enumerate(objects):
                # ignore dupli children
                if ob_main.parent and ob_main.parent.dupli_type in {'VERTS', 'FACES'}:
                    # XXX
                    continue
                obs = []
                if ob_main.dupli_type != 'NONE':
                    # XXX
                    print('creating dupli_list on', ob_main.name)
                    ob_main.dupli_list_create(scene)
                    obs = [(dob.object, dob.matrix) for dob in ob_main.dupli_list]
                    # XXX debug print
                    print(ob_main.name, 'has', len(obs), 'dupli children')
                else:
                    obs = [(ob_main, ob_main.matrix_world)]
                for ob, ob_mat in obs:   
                    if ob.type == 'MESH':
                        fw('%d %s %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f\n' % (frame, ob.name, ob_mat[0][0], ob_mat[1][0], ob_mat[2][0], ob_mat[3][0], ob_mat[0][1], ob_mat[1][1], ob_mat[2][1], ob_mat[3][1], ob_mat[0][2], ob_mat[1][2], ob_mat[2][2], ob_mat[3][2], ob_mat[0][3], ob_mat[1][3], ob_mat[2][3], ob_mat[3][3]))            
#                        fw('%d %s %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f\n' % (frame, ob.name, ob_mat[0][0], ob_mat[0][1], ob_mat[0][2], ob_mat[0][3], ob_mat[1][0], ob_mat[1][1], ob_mat[1][2], ob_mat[1][3], ob_mat[2][0], ob_mat[2][1], ob_mat[2][2], ob_mat[2][3], ob_mat[3][0], ob_mat[3][1], ob_mat[3][2], ob_mat[3][3]))            

def dump(obj):
   for attr in dir(obj):
       if hasattr( obj, attr ):
           print( "obj.%s = %s" % (attr, getattr(obj, attr)))
           
def write_bones(context, scene, filepath, MYOBJECTS):
    myfile = open(filepath, "w", encoding="utf8", newline="\n")
    for object in MYOBJECTS:
        if object.type != 'ARMATURE': continue
#        myfile.write("armature %s\n" % object.name)

#        skelet = object.data
#        print (skelet)
#        bones = skelet.bones.values()

        scene_frames = range(scene.frame_start, scene.frame_end + 1)  # Up to and including the end frame.
        for frame in scene_frames:
            scene.frame_set(frame, 0.0)

            for pbone in object.pose.bones:
                if pbone.parent:
                    parentName = pbone.parent.name;
#                    boneMat = pbone.matrix*pbone.parent.matrix.inverted()
                    boneMat = object.matrix_local*pbone.matrix
                else:
                    parentName = '';
                    boneMat = object.matrix_local*pbone.matrix
                    if frame==1:
                        print(object.name, object.matrix_world, pbone.name, pbone.matrix)

                myfile.write ('%i %s %s %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f\n' % (frame, pbone.name, parentName,
                         boneMat[0][0], boneMat[1][0], boneMat[2][0], boneMat[3][0], 
                         boneMat[0][1], boneMat[1][1], boneMat[2][1], boneMat[3][1], 
                         boneMat[0][2], boneMat[1][2], boneMat[2][2], boneMat[3][2], 
                         boneMat[0][3], boneMat[1][3], boneMat[2][3], boneMat[3][3]))            

def write_file(filepath, objects, scene,
               EXPORT_TRI=False,
               EXPORT_EDGES=False,
               EXPORT_SMOOTH_GROUPS=False,
               EXPORT_SMOOTH_GROUPS_BITFLAGS=False,
               EXPORT_NORMALS=False,
               EXPORT_UV=True,
               EXPORT_MTL=True,
               EXPORT_APPLY_MODIFIERS=True,
               EXPORT_BLEN_OBS=True,
               EXPORT_GROUP_BY_OB=False,
               EXPORT_GROUP_BY_MAT=False,
               EXPORT_KEEP_VERT_ORDER=False,
               EXPORT_POLYGROUPS=False,
               EXPORT_CURVE_AS_NURBS=True,
               EXPORT_GLOBAL_MATRIX=None,
               EXPORT_PATH_MODE='AUTO',
               progress=ProgressReport(),
               ):
    """
    Basic write function. The context and options must be already set
    This can be accessed externaly
    eg.
    write( 'c:\\test\\foobar.obj', Blender.Object.GetSelected() ) # Using default options.
    """
    if EXPORT_GLOBAL_MATRIX is None:
        EXPORT_GLOBAL_MATRIX = mathutils.Matrix()

    def veckey3d(v):
        return round(v.x, 4), round(v.y, 4), round(v.z, 4)

    def veckey2d(v):
        return round(v[0], 4), round(v[1], 4)

    def findVertexGroupName(face, vWeightMap):
        """
        Searches the vertexDict to see what groups is assigned to a given face.
        We use a frequency system in order to sort out the name because a given vetex can
        belong to two or more groups at the same time. To find the right name for the face
        we list all the possible vertex group names with their frequency and then sort by
        frequency in descend order. The top element is the one shared by the highest number
        of vertices is the face's group
        """
        weightDict = {}
        for vert_index in face.vertices:
            vWeights = vWeightMap[vert_index]
            for vGroupName, weight in vWeights:
                weightDict[vGroupName] = weightDict.get(vGroupName, 0.0) + weight

        if weightDict:
            return max((weight, vGroupName) for vGroupName, weight in weightDict.items())[1]
        else:
            return '(null)'

    # This contains vertex indices and weights for the vertices that belong
    # to this bone's group.  Also calculates the bone skin matrix.
    class _BoneVertexGroup:
            def __init__(self, BlenderObject, ArmatureObject, BoneName):
                self.BoneName = BoneName
#                self.SafeName = Util.SafeName(ArmatureObject.name) + "_" + Util.SafeName(BoneName)
                self.SafeName = BoneName
                
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
            _MeshEnumerator.__init__(self, Mesh)
            
            self.vertices = Mesh.vertices
            self.PolygonVertexIndices = tuple(tuple(Polygon.vertices)
                for Polygon in Mesh.polygons)
                
    def ExportWeights(mx):
        ArmatureModifierList = [Modifier 
            for Modifier in ob.modifiers
            if Modifier.type == 'ARMATURE' and Modifier.show_viewport]
        if not ArmatureModifierList:
            return
                                    
        # Skin weights work well with vertex reuse per face.  Use a
        # _OneToOneMeshEnumerator if possible.
        MeshEnumerator = _OneToOneMeshEnumerator(me)

        # Although multiple armature objects are gathered, support for
        # multiple armatures per mesh is not complete
        ArmatureObjects = [Modifier.object for Modifier in ArmatureModifierList]
        
        for ArmatureObject in ArmatureObjects:
            # Determine the names of the bone vertex groups
            PoseBoneNames = [Bone.name for Bone in ArmatureObject.pose.bones]
            VertexGroupNames = [Group.name for Group
                in ob.vertex_groups]
            UsedBoneNames = set(PoseBoneNames).intersection(VertexGroupNames)
            
            # Create a _BoneVertexGroup for each group name
            BoneVertexGroups = [_BoneVertexGroup(ob,
                ArmatureObject, BoneName) for BoneName in UsedBoneNames]
            
            # Maps Blender's internal group indexing to our _BoneVertexGroups
            GroupIndexToBoneVertexGroups = {Group.index : BoneVertexGroup
                for Group in ob.vertex_groups
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
                    BoneVertexGroup = GroupIndexToBoneVertexGroups.get(VertexGroup.group)
                    if BoneVertexGroup is not None:
                        Weight = VertexGroup.weight / VertexWeightTotal
                        BoneVertexGroup.AddVertex(Index, Weight)

        print('BoneVertexGroups', len(BoneVertexGroups))
        for BoneVertexGroup in BoneVertexGroups:
            fw("b ")
            fw(BoneVertexGroup.SafeName)
            boneMat = mx.inverted()
            fw(' %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f\n' % (
                     boneMat[0][0], boneMat[1][0], boneMat[2][0], boneMat[3][0], 
                     boneMat[0][1], boneMat[1][1], boneMat[2][1], boneMat[3][1], 
                     boneMat[0][2], boneMat[1][2], boneMat[2][2], boneMat[3][2], 
                     boneMat[0][3], boneMat[1][3], boneMat[2][3], boneMat[3][3]))            
            fw("vw")
            # Write the weights of the affected vertices.
            for Index, (VertexIndex, VertexWeight) in enumerate(zip(BoneVertexGroup.Indices, BoneVertexGroup.Weights)):
                fw(' %d/%f' % (VertexIndex, VertexWeight))
            fw("\n")
                            

    with ProgressReportSubstep(progress, 2, "OBJ Export path: %r" % filepath, "OBJ Export Finished") as subprogress1:
        with open(filepath, "w", encoding="utf8", newline="\n") as f:
            fw = f.write

            # Write Header
            fw('# Blender v%s OBJ File: %r\n' % (bpy.app.version_string, os.path.basename(bpy.data.filepath)))
            fw('# www.blender.org\n')

            # Tell the obj file what material file to use.
            if EXPORT_MTL:
                mtlfilepath = os.path.splitext(filepath)[0] + ".mtl"
                # filepath can contain non utf8 chars, use repr
                fw('mtllib %s\n' % repr(os.path.basename(mtlfilepath))[1:-1])

            # Initialize totals, these are updated each object
            totverts = totuvco = totno = 1

            face_vert_index = 1

            # A Dict of Materials
            # (material.name, image.name):matname_imagename # matname_imagename has gaps removed.
            mtl_dict = {}
            # Used to reduce the usage of matname_texname materials, which can become annoying in case of
            # repeated exports/imports, yet keeping unique mat names per keys!
            # mtl_name: (material.name, image.name)
            mtl_rev_dict = {}

            copy_set = set()

            # Get all meshes
            subprogress1.enter_substeps(len(objects))
            for i, ob_main in enumerate(objects):
                #print('enumerate(objects)', i, ob_main)
                # ignore dupli children
                if ob_main.parent and ob_main.parent.dupli_type in {'VERTS', 'FACES'}:
                    # XXX
                    subprogress1.step("Ignoring %s, dupli child..." % ob_main.name)
                    continue

                obs = []
                if ob_main.dupli_type != 'NONE':
                    # XXX
                    print('creating dupli_list on', ob_main.name)
                    ob_main.dupli_list_create(scene)

                    obs = [(dob.object, dob.matrix) for dob in ob_main.dupli_list]

                    # XXX debug print
                    print(ob_main.name, 'has', len(obs), 'dupli children')
                else:
                    obs = [(ob_main, ob_main.matrix_world)]

                subprogress1.enter_substeps(len(obs))
                for ob, ob_mat in obs:
                    with ProgressReportSubstep(subprogress1, 6) as subprogress2:
                        uv_unique_count = no_unique_count = 0

                        # Nurbs curve support
                        if EXPORT_CURVE_AS_NURBS and test_nurbs_compat(ob):
                            ob_mat = EXPORT_GLOBAL_MATRIX * ob_mat
                            totverts += write_nurb(fw, ob, ob_mat)
                            continue
                        # END NURBS

                        try:
                            me = ob.to_mesh(scene, EXPORT_APPLY_MODIFIERS, 'PREVIEW', calc_tessface=False)
                        except RuntimeError:
                            me = None

                        if me is None:
                            continue

 #                       me.transform(EXPORT_GLOBAL_MATRIX * ob_mat)
                                    
                        if EXPORT_TRI:
                            # _must_ do this first since it re-allocs arrays
                            mesh_triangulate(me)

                        if EXPORT_UV:
                            faceuv = len(me.uv_textures) > 0
                            if faceuv:
                                uv_texture = me.uv_textures.active.data[:]
                                uv_layer = me.uv_layers.active.data[:]
                        else:
                            faceuv = False

                        me_verts = me.vertices[:]

                        # Make our own list so it can be sorted to reduce context switching
                        face_index_pairs = [(face, index) for index, face in enumerate(me.polygons)]
                        # faces = [ f for f in me.tessfaces ]

                        if EXPORT_EDGES:
                            edges = me.edges
                        else:
                            edges = []

                        if not (len(face_index_pairs) + len(edges) + len(me.vertices)):  # Make sure there is something to write
                            # clean up
                            bpy.data.meshes.remove(me)
                            continue  # dont bother with this mesh.

                        if EXPORT_NORMALS and face_index_pairs:
                            me.calc_normals_split()
                            # No need to call me.free_normals_split later, as this mesh is deleted anyway!
                            loops = me.loops
                        else:
                            loops = []

                        if (EXPORT_SMOOTH_GROUPS or EXPORT_SMOOTH_GROUPS_BITFLAGS) and face_index_pairs:
                            smooth_groups, smooth_groups_tot = me.calc_smooth_groups(EXPORT_SMOOTH_GROUPS_BITFLAGS)
                            if smooth_groups_tot <= 1:
                                smooth_groups, smooth_groups_tot = (), 0
                        else:
                            smooth_groups, smooth_groups_tot = (), 0

                        materials = me.materials[:]
                        material_names = [m.name if m else None for m in materials]

                        # avoid bad index errors
                        if not materials:
                            materials = [None]
                            material_names = [name_compat(None)]

                        # Sort by Material, then images
                        # so we dont over context switch in the obj file.
                        if EXPORT_KEEP_VERT_ORDER:
                            pass
                        else:
                            if faceuv:
                                if smooth_groups:
                                    sort_func = lambda a: (a[0].material_index,
                                                           hash(uv_texture[a[1]].image),
                                                           smooth_groups[a[1]] if a[0].use_smooth else False)
                                else:
                                    sort_func = lambda a: (a[0].material_index,
                                                           hash(uv_texture[a[1]].image),
                                                           a[0].use_smooth)
                            elif len(materials) > 1:
                                if smooth_groups:
                                    sort_func = lambda a: (a[0].material_index,
                                                           smooth_groups[a[1]] if a[0].use_smooth else False)
                                else:
                                    sort_func = lambda a: (a[0].material_index,
                                                           a[0].use_smooth)
                            else:
                                # no materials
                                if smooth_groups:
                                    sort_func = lambda a: smooth_groups[a[1] if a[0].use_smooth else False]
                                else:
                                    sort_func = lambda a: a[0].use_smooth

                            face_index_pairs.sort(key=sort_func)

                            del sort_func

                        # Set the default mat to no material and no image.
                        contextMat = 0, 0  # Can never be this, so we will label a new material the first chance we get.
                        contextSmooth = None  # Will either be true or false,  set bad to force initialization switch.

                        if EXPORT_BLEN_OBS or EXPORT_GROUP_BY_OB:
                            name1 = ob.name
                            name2 = ob.data.name
                            if name1 == name2:
                                obnamestring = name_compat(name1)
                            else:
                                obnamestring = '%s_%s' % (name_compat(name1), name_compat(name2))

                            if EXPORT_BLEN_OBS:
                                fw('o %s\n' % obnamestring)  # Write Object name
                            else:  # if EXPORT_GROUP_BY_OB:
                                fw('g %s\n' % obnamestring)

                        subprogress2.step()

                        # Vert
                        for v in me_verts:
                            fw('v %.6f %.6f %.6f\n' % v.co[:])
                        
                        subprogress2.step()

                        # UV
                        if faceuv:
                            # in case removing some of these dont get defined.
                            uv = f_index = uv_index = uv_key = uv_val = uv_ls = None

                            uv_face_mapping = [None] * len(face_index_pairs)

                            uv_dict = {}
                            uv_get = uv_dict.get
                            for f, f_index in face_index_pairs:
                                uv_ls = uv_face_mapping[f_index] = []
                                for uv_index, l_index in enumerate(f.loop_indices):
                                    uv = uv_layer[l_index].uv
                                    uv_key = veckey2d(uv)
                                    uv_val = uv_get(uv_key)
                                    if uv_val is None:
                                        uv_val = uv_dict[uv_key] = uv_unique_count
                                        fw('vt %.6f %.6f\n' % uv[:])
                                        uv_unique_count += 1
                                    uv_ls.append(uv_val)

                            del uv_dict, uv, f_index, uv_index, uv_ls, uv_get, uv_key, uv_val
                            # Only need uv_unique_count and uv_face_mapping

                        subprogress2.step()

                        # NORMAL, Smooth/Non smoothed.
                        if EXPORT_NORMALS:
                            no_key = no_val = None
                            normals_to_idx = {}
                            no_get = normals_to_idx.get
                            loops_to_normals = [0] * len(loops)
                            for f, f_index in face_index_pairs:
                                for l_idx in f.loop_indices:
                                    no_key = veckey3d(loops[l_idx].normal)
                                    no_val = no_get(no_key)
                                    if no_val is None:
                                        no_val = normals_to_idx[no_key] = no_unique_count
                                        fw('vn %.6f %.6f %.6f\n' % no_key)
                                        no_unique_count += 1
                                    loops_to_normals[l_idx] = no_val
                            del normals_to_idx, no_get, no_key, no_val
                        else:
                            loops_to_normals = []

                        if not faceuv:
                            f_image = None

                        subprogress2.step()

                        # XXX
                        if EXPORT_POLYGROUPS:
                            # Retrieve the list of vertex groups
                            vertGroupNames = ob.vertex_groups.keys()
                            if vertGroupNames:
                                currentVGroup = ''
                                # Create a dictionary keyed by face id and listing, for each vertex, the vertex groups it belongs to
                                vgroupsMap = [[] for _i in range(len(me_verts))]
                                for v_idx, v_ls in enumerate(vgroupsMap):
                                    v_ls[:] = [(vertGroupNames[g.group], g.weight) for g in me_verts[v_idx].groups]

                        for f, f_index in face_index_pairs:
                            f_smooth = f.use_smooth
                            if f_smooth and smooth_groups:
                                f_smooth = smooth_groups[f_index]
                            f_mat = min(f.material_index, len(materials) - 1)

                            if faceuv:
                                tface = uv_texture[f_index]
                                f_image = tface.image

                            # MAKE KEY
                            if faceuv and f_image:  # Object is always true.
                                key = material_names[f_mat], f_image.name
                            else:
                                key = material_names[f_mat], None  # No image, use None instead.

                            # Write the vertex group
                            if EXPORT_POLYGROUPS:
                                if vertGroupNames:
                                    # find what vertext group the face belongs to
                                    vgroup_of_face = findVertexGroupName(f, vgroupsMap)
                                    if vgroup_of_face != currentVGroup:
                                        currentVGroup = vgroup_of_face
                                        fw('g %s\n' % vgroup_of_face)

                            # CHECK FOR CONTEXT SWITCH
                            if key == contextMat:
                                pass  # Context already switched, dont do anything
                            else:
                                if key[0] is None and key[1] is None:
                                    # Write a null material, since we know the context has changed.
                                    if EXPORT_GROUP_BY_MAT:
                                        # can be mat_image or (null)
                                        fw("g %s_%s\n" % (name_compat(ob.name), name_compat(ob.data.name)))
                                    if EXPORT_MTL:
                                        fw("usemtl (null)\n")  # mat, image

                                else:
                                    mat_data = mtl_dict.get(key)
                                    if not mat_data:
                                        # First add to global dict so we can export to mtl
                                        # Then write mtl

                                        # Make a new names from the mat and image name,
                                        # converting any spaces to underscores with name_compat.

                                        # If none image dont bother adding it to the name
                                        # Try to avoid as much as possible adding texname (or other things)
                                        # to the mtl name (see [#32102])...
                                        mtl_name = "%s" % name_compat(key[0])
                                        if mtl_rev_dict.get(mtl_name, None) not in {key, None}:
                                            if key[1] is None:
                                                tmp_ext = "_NONE"
                                            else:
                                                tmp_ext = "_%s" % name_compat(key[1])
                                            i = 0
                                            while mtl_rev_dict.get(mtl_name + tmp_ext, None) not in {key, None}:
                                                i += 1
                                                tmp_ext = "_%3d" % i
                                            mtl_name += tmp_ext
                                        mat_data = mtl_dict[key] = mtl_name, materials[f_mat], f_image
                                        mtl_rev_dict[mtl_name] = key

                                    if EXPORT_GROUP_BY_MAT:
                                        # can be mat_image or (null)
                                        fw("g %s_%s_%s\n" % (name_compat(ob.name), name_compat(ob.data.name), mat_data[0]))
                                    if EXPORT_MTL:
                                        fw("usemtl %s\n" % mat_data[0])  # can be mat_image or (null)

                            contextMat = key
                            if f_smooth != contextSmooth:
                                if f_smooth:  # on now off
                                    if smooth_groups:
                                        f_smooth = smooth_groups[f_index]
                                        fw('s %d\n' % f_smooth)
                                    else:
                                        fw('s 1\n')
                                else:  # was off now on
                                    fw('s off\n')
                                contextSmooth = f_smooth

                            f_v = [(vi, me_verts[v_idx], l_idx)
                                   for vi, (v_idx, l_idx) in enumerate(zip(f.vertices, f.loop_indices))]

                            fw('f')
                            if faceuv:
                                if EXPORT_NORMALS:
                                    for vi, v, li in f_v:
                                        fw(" %d/%d/%d" % (totverts + v.index,
                                                          totuvco + uv_face_mapping[f_index][vi],
                                                          totno + loops_to_normals[li],
                                                          ))  # vert, uv, normal
                                else:  # No Normals
                                    for vi, v, li in f_v:
                                        fw(" %d/%d" % (totverts + v.index,
                                                       totuvco + uv_face_mapping[f_index][vi],
                                                       ))  # vert, uv

                                face_vert_index += len(f_v)

                            else:  # No UV's
                                if EXPORT_NORMALS:
                                    for vi, v, li in f_v:
                                        fw(" %d//%d" % (totverts + v.index, totno + loops_to_normals[li]))
                                else:  # No Normals
                                    for vi, v, li in f_v:
                                        fw(" %d" % (totverts + v.index))

                            fw('\n')

                        ExportWeights(ob.matrix_world)
                                
                        subprogress2.step()

                        # Write edges.
                        if EXPORT_EDGES:
                            for ed in edges:
                                if ed.is_loose:
                                    fw('l %d %d\n' % (totverts + ed.vertices[0], totverts + ed.vertices[1]))

                        # Make the indices global rather then per mesh
                        totverts += len(me_verts)
                        totuvco += uv_unique_count
                        totno += no_unique_count

                        # clean up
                        bpy.data.meshes.remove(me)

                if ob_main.dupli_type != 'NONE':
                    ob_main.dupli_list_clear()

                subprogress1.leave_substeps("Finished writing geometry of '%s'." % ob_main.name)
            subprogress1.leave_substeps()

        subprogress1.step("Finished exporting geometry, now exporting materials")

        # Now we have all our materials, save them
        if EXPORT_MTL:
            write_mtl(scene, mtlfilepath, EXPORT_PATH_MODE, copy_set, mtl_dict)

        # copy all collected files.
        bpy_extras.io_utils.path_reference_copy(copy_set)

def _write(context, filepath,
           EXPORT_TRI,  # ok
           EXPORT_EDGES,
           EXPORT_SMOOTH_GROUPS,
           EXPORT_SMOOTH_GROUPS_BITFLAGS,
           EXPORT_NORMALS,  # ok
           EXPORT_UV,  # ok
           EXPORT_MTL,
           EXPORT_APPLY_MODIFIERS,  # ok
           EXPORT_BLEN_OBS,
           EXPORT_GROUP_BY_OB,
           EXPORT_GROUP_BY_MAT,
           EXPORT_KEEP_VERT_ORDER,
           EXPORT_POLYGROUPS,
           EXPORT_CURVE_AS_NURBS,
           EXPORT_SEL_ONLY,  # ok
           EXPORT_ANIMATION,
           EXPORT_GLOBAL_MATRIX,
           EXPORT_PATH_MODE,  # Not used
           ):

    with ProgressReport(context.window_manager) as progress:
        base_name, ext = os.path.splitext(filepath)
        context_name = [base_name, '', '', ext]  # Base name, scene name, frame number, extension

        scene = context.scene

        # Exit edit mode before exporting, so current object states are exported properly.
        if bpy.ops.object.mode_set.poll():
            bpy.ops.object.mode_set(mode='OBJECT')

        orig_frame = scene.frame_current

        # Export an animation?
        if EXPORT_ANIMATION:
            scene_frames = range(scene.frame_start, scene.frame_end + 1)  # Up to and including the end frame.
        else:
            scene_frames = [orig_frame]  # Dont export an animation.

        # Loop through all frames in the scene and export.
        progress.enter_substeps(len(scene_frames))
        for frame in scene_frames:
            print('frame', frame)
            if EXPORT_ANIMATION:  # Add frame to the filepath.
                context_name[2] = '_%.6d' % frame

            scene.frame_set(frame, 0.0)
            if EXPORT_SEL_ONLY:
                objects = context.selected_objects
            else:
                objects = scene.objects

            full_path = ''.join(context_name)

            # erm... bit of a problem here, this can overwrite files when exporting frames. not too bad.
            # EXPORT THE FILE.
            progress.enter_substeps(1)
            write_file(full_path, objects, scene,
                       EXPORT_TRI,
                       EXPORT_EDGES,
                       EXPORT_SMOOTH_GROUPS,
                       EXPORT_SMOOTH_GROUPS_BITFLAGS,
                       EXPORT_NORMALS,
                       EXPORT_UV,
                       EXPORT_MTL,
                       EXPORT_APPLY_MODIFIERS,
                       EXPORT_BLEN_OBS,
                       EXPORT_GROUP_BY_OB,
                       EXPORT_GROUP_BY_MAT,
                       EXPORT_KEEP_VERT_ORDER,
                       EXPORT_POLYGROUPS,
                       EXPORT_CURVE_AS_NURBS,
                       EXPORT_GLOBAL_MATRIX,
                       EXPORT_PATH_MODE,
                       progress,
                       )
            write_obj_matrix(context, scene, os.path.splitext(filepath)[0] + ".obja", objects)
            write_bones(context, scene, os.path.splitext(filepath)[0] + ".objb", objects)

            progress.leave_substeps()

        scene.frame_set(orig_frame, 0.0)
        progress.leave_substeps()

"""
Currently the exporter lacks these features:
* multiple scene export (only active scene is written)
* particles
"""


def save(context, filepath="",
         use_triangles=False,
         use_edges=True,
         use_normals=True,
         use_smooth_groups=False,
         use_smooth_groups_bitflags=False,
         use_uvs=True,
         use_materials=True,
         use_mesh_modifiers=True,
         use_blen_objects=True,
         group_by_object=False,
         group_by_material=False,
         keep_vertex_order=False,
         use_vertex_groups=False,
         use_nurbs=False,
         use_selection=False,
         use_animation=False,
         global_matrix=None,
         path_mode='STRIP'
         ):

    print('filepath ', filepath)
    _write(context, filepath,
           EXPORT_TRI=use_triangles,
           EXPORT_EDGES=use_edges,
           EXPORT_SMOOTH_GROUPS=use_smooth_groups,
           EXPORT_SMOOTH_GROUPS_BITFLAGS=use_smooth_groups_bitflags,
           EXPORT_NORMALS=use_normals,
           EXPORT_UV=use_uvs,
           EXPORT_MTL=use_materials,
           EXPORT_APPLY_MODIFIERS=use_mesh_modifiers,
           EXPORT_BLEN_OBS=use_blen_objects,
           EXPORT_GROUP_BY_OB=group_by_object,
           EXPORT_GROUP_BY_MAT=group_by_material,
           EXPORT_KEEP_VERT_ORDER=keep_vertex_order,
           EXPORT_POLYGROUPS=use_vertex_groups,
           EXPORT_CURVE_AS_NURBS=use_nurbs,
           EXPORT_SEL_ONLY=use_selection,
           EXPORT_ANIMATION=use_animation,
           EXPORT_GLOBAL_MATRIX=global_matrix,
           EXPORT_PATH_MODE=path_mode,
           )

    return {'FINISHED'}

save(bpy.context, bpy.path.abspath("//model.obj"))