#!/usr/bin/env python

"""
This script prints the C definitions of the items found in a PDB file.

The script works correctly for nearly all definitions, but it does fail on
more complex structures/unions containing nested structs/unions. The sizes and
offsets used by this script seem to be correct, but sometimes it just can't
make sense of them. Here's an example failure (from Vista sp1 32 bit - ntdll.pdb):

struct _DISPATCHER_HEADER { // 0x10 bytes
 // Size vs offset misalignment at offset 0x1 (curr size 0x4)
 // Size vs offset misalignment at offset 0x2 (curr size 0x5)
 // Size vs offset misalignment at offset 0x3 (curr size 0x6)
 // Size vs offset misalignment at offset 0x4 (curr size 0x7)
 // ************ INCORRECT SIZE *************************
 // claimed in PDB: 0x10, calculated: 0x13
    /*
    union {
       UCHAR Type;                                        // offset   0x0 size   0x1
       volatile LONG Lock;                                // offset   0x0 size   0x4
    };
    union {
       UCHAR Abandoned;                                   // offset   0x1 size   0x1
       UCHAR Absolute;                                    // offset   0x1 size   0x1
       UCHAR NpxIrql;                                     // offset   0x1 size   0x1
       UCHAR Signalling;                                  // offset   0x1 size   0x1
    };
    union {
       UCHAR Size;                                        // offset   0x2 size   0x1
       UCHAR Hand;                                        // offset   0x2 size   0x1
    };
    union {
       UCHAR Inserted;                                    // offset   0x3 size   0x1
       UCHAR DebugActive;                                 // offset   0x3 size   0x1
       UCHAR DpcActive;                                   // offset   0x3 size   0x1
    };
    LONG SignalState;                                  // offset   0x4 size   0x4
    LIST_ENTRY WaitListHead;                           // offset   0x8 size   0x8
    */
    UINT8 blob[0x10]; // print_ctypes.py validation failure
 
 } __attribute__((packed));


The items at common offsets (e.g. 0x1) should probably be in separate
structures within a union.
"""

import sys
import os
import pdbparse
import random
import pprint

# Topological sort, by Paul Harrison
# Found at:
#   http://www.logarithmic.net/pfh-files/blog/01208083168/sort.py
# License: Public domain
def topological_sort(graph):
    count = { }
    for node in graph:
        count[node] = 0
    for node in graph:
        for successor in graph[node]:
            count[successor] += 1

    ready = [ node for node in graph if count[node] == 0 ]
    
    result = [ ]
    while ready:
        node = ready.pop(-1)
        result.append(node)
        
        for successor in graph[node]:
            count[successor] -= 1
            if count[successor] == 0:
                ready.append(successor)
    
    return result

def rand_str(length):
    alphabet = "abcdefghijklmnopqrstuvwxyz"
    alphabet += alphabet.upper()
    return "".join(random.sample(alphabet,length))


ARCH_PTR_SIZE = None

snames = {
    "LF_STRUCTURE": "struct",
    "LF_ENUM": "enum",
    "LF_UNION": "union",
}

indent = ' ' * 3
ctype = {}
ptr_str = None
fptr_str = None
struct_pretty_str = None

# Microsoft Visual Studio "theme"
ctype_msvc  = {
    "T_32PINT4": "PLONG",
    "T_32PRCHAR": "PUCHAR",
    "T_32PUCHAR": "PUCHAR",
    "T_32PULONG": "PULONG",
    "T_32PLONG": "PLONG",
    "T_32PUQUAD": "PULONGLONG",
    "T_32PUSHORT": "PUSHORT",
    "T_32PVOID": "PVOID",
    "T_64PULONG": "PULONG",
    "T_64PRCHAR": "PUCHAR",
    "T_64PUCHAR": "PUCHAR",
    "T_64PULONG": "PULONG",
    "T_64PLONG": "PLONG",
    "T_64PUQUAD": "PULONGLONG",
    "T_64PQUAD": "PLONGLONG",
    "T_64PUSHORT": "PUSHORT",
    "T_64PVOID": "PVOID64",
    "T_INT4": "LONG",
    "T_INT8": "LONGLONG",
    "T_LONG": "LONG",
    "T_QUAD": "LONGLONG",
    "T_RCHAR": "UCHAR",
    "T_REAL32": "FLOAT",
    "T_REAL64": "DOUBLE",
    "T_REAL80": "long double",
    "T_SHORT": "SHORT",
    "T_UCHAR": "UCHAR",
    "T_UINT4": "ULONG",
    "T_ULONG": "ULONG",
    "T_UQUAD": "ULONGLONG",
    "T_USHORT": "USHORT",
    "T_WCHAR": "WCHAR",
    "T_VOID": "VOID",
}

# Introspection "theme" for a 32-bit target
ctype_intro  = {
    "T_32PINT4": "uint32_t",
    "T_32PRCHAR": "uint32_t",
    "T_32PUCHAR": "uint32_t",
    "T_32PULONG": "uint32_t",
    "T_32PLONG": "uint32_t",
    "T_32PUQUAD": "uint32_t",
    "T_32PUSHORT": "uint32_t",
    "T_32PVOID": "uint32_t",
    "T_64PVOID": "uint64_t",
    "T_INT4": "int32_t",
    "T_INT8": "int64_t",
    "T_LONG": "int32_t",
    "T_QUAD": "int64_t",
    "T_RCHAR": "uint8_t",
    "T_REAL32": "float",
    "T_REAL64": "double",
    "T_REAL80": "long double",
    "T_SHORT": "int16_t",
    "T_UCHAR": "uint8_t",
    "T_UINT4": "uint32_t",
    "T_ULONG": "uint32_t",
    "T_UQUAD": "uint64_t",
    "T_USHORT": "uint16_t",
    "T_WCHAR": "uint16_t",
    "T_VOID": "void",
}

def print_basic_types():
    print "/******* Define basic Windows types *******/"
    print
    print "// If compiling with gcc, use -fms-extensions"
    print
    print "#include <stdint.h>"
    print
    print "typedef  uint8_t     UINT8;"
    print "typedef  uint8_t     UCHAR;"
    print "typedef  uint8_t      BOOL;"
    print
    print "typedef   int8_t      CHAR;"
    print "typedef   int8_t      INT8;"
    print
    print "typedef uint16_t     WCHAR;"
    print "typedef uint16_t    UINT16;"
    print "typedef uint16_t    USHORT;"
    print "typedef  int16_t     SHORT;"
    print
    print "typedef uint32_t    UINT32;"
    print "typedef uint32_t     ULONG;"
    print "typedef  int32_t      LONG;"
    print
    print "typedef uint64_t    UINT64;"
    print "typedef uint64_t ULONGLONG;"
    print "typedef  int64_t  LONGLONG;"
    print
    print "typedef uint64_t   PVOID64, PPVOID64;"
    print "typedef uint32_t   PVOID32, PPVOID32;"
    print "typedef     void      VOID;"
    print
    print "typedef  double     DOUBLE; // not true but -hey FIXME"
    print
    print "#ifdef WINDOWS_USE_32_BIT_POINTERS ///////////////"
    print "// pointers occupy exactly 32 bits"
    print "typedef  UINT32     PUINT8;"
    print "typedef  UINT32     PUCHAR;"
    print "typedef  UINT32      PBOOL;"
    print
    print "typedef  UINT32      PCHAR;"
    print "typedef  UINT32      PINT8;"
    print
    print "typedef  UINT32    PUINT16;"
    print "typedef  UINT32    PUSHORT;"
    print "typedef  UINT32     PSHORT;"
    print
    print "typedef  UINT32     PUINT32;"
    print "typedef  UINT32      PULONG;"
    print "typedef  UINT32       PLONG;"
    print
    print "typedef  UINT32     PUINT64;"
    print "typedef  UINT32  PULONGLONG;"
    print "typedef  UINT32   PLONGLONG;"
    print
    print "typedef  UINT32       PVOID, PPVOID;"
    print
    print "#else /////////////////  !WINDOWS_USE_32_BIT_POINTERS"
    print "// pointers occupy native address width per ABI"
    print "typedef     UINT8     *PUINT8;"
    print "typedef     UCHAR     *PUCHAR;"
    print "typedef      BOOL      *PBOOL;"
    print
    print "typedef      CHAR      *PCHAR;"
    print "typedef      INT8      *PINT8;"
    print
    print "typedef    UINT16    *PUINT16;"
    print "typedef    USHORT    *PUSHORT;"
    print "typedef     SHORT     *PSHORT;"
    print
    print "typedef    UINT32    *PUINT32;"
    print "typedef     ULONG     *PULONG;"
    print "typedef      LONG      *PLONG;"
    print
    print "typedef    UINT64    *PUINT64;"
    print "typedef ULONGLONG *PULONGLONG;"
    print "typedef  LONGLONG  *PLONGLONG;"
    print
    print "typedef      VOID      *PVOID, **PPVOID;"
    print
    print "#endif /////////////////  WINDOWS_USE_32_BIT_POINTERS"
    print "\n\n\n"
    print "#define P(basetype, var) ( (basetype *)(var))"
    print "\n\n\n"

    
base_type_size = {
    "T_32PRCHAR":  4,
    "T_32PUCHAR":  4,
    "T_32PULONG":  4,
    "T_32PUQUAD":  4,
    "T_32PUSHORT": 4,
    "T_32PVOID":   4,
    "T_32PLONG":   4,
    
    "T_64PRCHAR":  8,
    "T_64PUCHAR":  8,
    "T_64PULONG":  8,
    "T_64PUQUAD":  8,
    "T_64PUSHORT": 8,
    "T_64PVOID":   8,
    "T_64PLONG":   8,

    "T_INT4":      4,
    "T_INT8":      8,
    "T_LONG":      4,
    "T_QUAD":      8,
    "T_RCHAR":     1,
    "T_REAL32":    4,
    "T_REAL64":    8,
    "T_REAL80":   10,
    "T_SHORT":     2,
    "T_UCHAR":     1,
    "T_UINT4":     4,
    "T_ULONG":     4,
    "T_UQUAD":     8,
    "T_USHORT":    2,
    "T_WCHAR":     2,
}

class Record(list):
    @property
    def ofs(self):
        return self[0].ofs
        
    def get_size(self):
        #return sum([el.get_size() for el in self])
        size = 0
        ofs = self.ofs
        # get the meximum offset+size of fields
        size = max([ ((el.ofs+el.get_size()) - ofs) for el in self])
        return size
        
        
    def to_string(self, tab='', typedef=False):
        ret =''
        if not typedef:
            ret = '%s%s %s{\n'%(tab, self.type, self.name)
        for el in self:
                ret += '%s\n'%(el.to_string(tab=tab+'\t'))
        if not typedef:
            ret += '%s};'%(tab)
        return ret

    def __str__(self):
        ret = '%s %s{\n'%(self.type, self.name)
        for el in self:
            ret += '\t%s\n'%str(el)
        ret += '};'
        return ret


class Union(Record):
    name = ''
    type = 'union'

class Struct(Record):
    name = ''
    type = 'struct'

class Member:
    """
    Describes one member of a structure or union. A member is a field received
    by the underlying parsing library or one created by this script (e.g. one
    that fills a gap found in a struct).
    """
    def __init__(self, ofs, size, alignment, leaf_type, contents, index, suppress_meta=False):
        '''
        ofs - offset where Member starts
        size - size of member; could be fractional if bitfield
        alignment - size of member, cannot be fractional

        e.g.        UINT Member1 : 3;
        size is 0.375 == 3/8 (byte), alignment is 1 (byte)
        '''
        self.ofs       = ofs
        self.size      = size
        self.alignment = alignment
        self.leaf_type = leaf_type
        self.contents  = contents
        self.index     = index
        self.suppress  = suppress_meta

    def __str__(self):
        if self.suppress:
            return self.member_str()

        else:
            return '{0:<50} {1}'.format(self.member_str(), self.comment_str())

    def to_string(self, tab):
        if self.suppress:
            return tab+self.member_str()

        else:
            return '{0:<50} {1}'.format(tab+self.member_str(), self.comment_str())
    
    def member_str(self):
        return self.contents

    def comment_str(self):
        # Not suppressing metadata, so calculate it now.
        if self.leaf_type == 'LF_BITFIELD':
            ofs_str  = '{0:>5}'.format(self.ofs)
            size_str = '{0:>5}'.format(self.size)

            if  self.ofs != 0 and int(self.ofs) == self.ofs: # ofs is an int, show it in hex too
                ofs_str += ' [{0:#x}]'.format(int(self.ofs))
        else: # not a bitfield - assume integer size & offset
            ofs_str  = '{0:>5}'.format(hex(int(self.ofs)))
            size_str = '{0:>5}'.format(hex(int(self.size)))
            
        # display member's metadata
        return '// offset {0} size {1}'.format(ofs_str, size_str)

    def get_size(self):
        return self.size
    
    def __len__(self):
        return self.size
    
class OneRun:
    """
    Describes one run. A run is a fundamental concept in this code -- it is a
    sequence of Member objects that appear consecutively (i.e. they have
    increasing offsets with no gaps between them).
    """
    def __init__(self, ofs):
        self.members   = list() # list consisting only of Members
        self.ofs       = ofs
        self.ltype     = 'LF_STRUCTURE'
        self.next_ofs  = ofs

    def next_ofs(self):
        return self.ofs + len(self)

    def __len__(self):
        return sum([x.size for x in self.members])

    def __str__(self):
        return ('// Sequential members from 0x%x to 0x%x (length 0x%x bytes)\n'
                % (self.ofs, self.next_ofs(), len(self)) +
                '\n'.join([str(m) for m in self.members]))

    def add_member(self,m):
        self.members.append(m)

    def add_members(self,ms):
        for m in ms:
            self.add_member(m)

def is_function_pointer(lf):
    if isinstance(lf, str):
        return False
    if lf.leaf_type == "LF_POINTER":
        return is_function_pointer(lf.utype)
    elif lf.leaf_type == "LF_PROCEDURE":
        return True
    else:
        return False

def is_inline_struct(lf):
    if isinstance(lf, str): return False
    if (lf.leaf_type == "LF_STRUCTURE" or
        lf.leaf_type == "LF_UNION" or
        lf.leaf_type == "LF_ENUM") and "unnamed" in lf.name:
        return True
    else:
        try:
            if "unnamed" in lf.name: print lf.leaf_type
        except:
            pass
    return False

def proc_arglist(proc):
    argstrs = []
    for a in proc.arglist.arg_type:
        argstrs.append(get_tpname(a))
    return argstrs

def fptr_str_intro(fptr,name):
    return "uint32_t %s" % name

def fptr_str_std(fptr,name):
    stars = ""
    while fptr.leaf_type == "LF_POINTER":
        stars += "*"
        fptr = fptr.utype
    ret_type = get_tpname(fptr.return_type)
    arglist = proc_arglist(fptr)
    return "%s (%s%s)(%s)" % (ret_type, stars, name, ", ".join(arglist))

def demangle(nm):
    if nm.startswith("_"): return nm[1:]
    else: return nm

def mangle(nm):
    if not nm.startswith("_"): return "_" + nm
    else: return nm

def get_size(lf):
    if isinstance(lf,str):
        return base_type_size[lf]
    elif (lf.leaf_type == "LF_STRUCTURE" or
          lf.leaf_type == "LF_ARRAY" or
          lf.leaf_type == "LF_UNION"):
        return lf.size
    elif lf.leaf_type == "LF_POINTER":
        return ARCH_PTR_SIZE
    elif lf.leaf_type == "LF_MODIFIER":
        return get_size(lf.modified_type)
    elif lf.leaf_type == "LF_ENUM":
        return get_size("T_INT4")
    elif lf.leaf_type == "LF_BITFIELD":
        return 1.0 * lf.length / 8
    else:
        print >>sys.stderr, "ERROR: don't know how to get size for %s" % lf.leaf_type
        return -1

def get_basetype(lf):
    if isinstance(lf, str):
        return None
    elif (lf.leaf_type == "LF_STRUCTURE" or
          lf.leaf_type == "LF_ENUM" or
          lf.leaf_type == "LF_UNION"):
        return lf
    elif lf.leaf_type == "LF_POINTER":
        return get_basetype(lf.utype)
    elif lf.leaf_type == "LF_ARRAY":
        return get_basetype(lf.element_type)
    elif lf.leaf_type == "LF_MODIFIER":
        return get_basetype(lf.modified_type)
    else:
        return None

def get_tpname(lf, name=None):
    if isinstance(lf, str):
        try: tpname = ctype[lf]
        except KeyError: tpname = lf
        if name: tpname += " " + name
    elif (lf.leaf_type == "LF_STRUCTURE" or
          lf.leaf_type == "LF_ENUM" or
          lf.leaf_type == "LF_UNION"):
        tpname = demangle(lf.name)
        if name: tpname += " " + name
    elif lf.leaf_type == "LF_POINTER":   tpname = ptr_str(lf,name)
    elif lf.leaf_type == "LF_PROCEDURE": tpname = proc_str(lf,name)
    elif lf.leaf_type == "LF_MODIFIER":  tpname = mod_str(lf,name)
    elif lf.leaf_type == "LF_ARRAY":     tpname = arr_str(lf,name)
    elif lf.leaf_type == "LF_BITFIELD":  tpname = bit_str(lf,name)
    else:
        tpname = lf.leaf_type
        if name: tpname += " " + name
    return tpname

def bit_str(bitf, name):
    return "%s %s : %d" % (get_tpname(bitf.base_type), name, bitf.length)

def arr_str(arr, name):
    tpname = get_tpname(arr.element_type)
    sz = get_size(arr.element_type)
    if sz == 0:
        print >>sys.stderr,"ERROR with array %s %s" % (tpname, name)
    if sz < 0:
        print >>sys.stderr,"ERROR with array %s %s -- element size is negative" % (tpname, name)
    if arr.size < 0:
        print >>sys.stderr,"ERROR with array %s %s -- size is negative" % (tpname, name)
    count = arr.size / sz
    return memb_str(arr.element_type, "%s[0x%x]" % (name,count))
    #return "%s %s[%d]" % (tpname, name, count)

def mod_str(mod, name):
    tpname = get_tpname(mod.modified_type)
    modifiers = [ m for m in ["const","unaligned","volatile"] if mod.modifier[m]]
    tpname = "%s %s" % (" ".join(modifiers), tpname)
    if name: tpname += " " + name
    return tpname

def ptr_str_intro(ptr, name):
    if name:
        return "uint32_t %s" % name
    else:
        return "uint32_t"

def ptr_str_std(ptr, name):
    tpname = get_tpname(ptr.utype)
    if name:
        # Without this hack, we can end up with a defn like
        # struct _TEB_ACTIVE_FRAME_CONTEXT { // 0x10 bytes
        #              ULONG Flags ; // offset 0x0
        #              Pconst UCHAR FrameName ; // offset 0x8 <--- not parseable!!!
        # };

        # Handle pointer with modifier - the modifier goes to the beginning
        mods = list()
        for m in ("const","unaligned","volatile"):
            if tpname.find(m) > -1:
                mods.append(m)
                tpname = tpname.replace(m + ' ', '')

        return ' '.join(mods) + (' ' if mods else '') + 'P%s %s' % (tpname, name)
    else:
        return "P%s" % tpname

def proc_str(proc, name):
    argstrs = proc_arglist(proc)
    ret_type = get_tpname(proc.return_type)
    if not name: name = "func_" + rand_str(5)
    return "%s (*%s)(%s)" % (ret_type, name, ", ".join(argstrs))

def memb_str(memb, name, off=-1):
    if is_function_pointer(memb):
        tpname = fptr_str(memb, name)
    elif is_inline_struct(memb):
        sname = snames[memb.leaf_type]
        tpname = sname + " {\n"
        tpname += flstr(memb)
        tpname += "} " + name
    else:
        tpname = get_tpname(memb, name)
    if off != -1:
        size = get_size(memb)
        ltype = None
        length = 0
        ofs_str = '%#x' % off
        size_str = ('%#x' % size ) 
        alignment = 0
        if not isinstance(memb,str):
            ltype = memb.leaf_type
            length = memb.length
            if memb.leaf_type == 'LF_BITFIELD':
                ofs_str = '%f' % off
                size_str = '%f' % size

                if isinstance(memb['base_type'], str):
                    btype = memb['base_type']
                else:
                    btype = memb['base_type']['modified_type']
                
                alignment = base_type_size[btype]

        return (off, size, alignment, ltype, "%s;" % tpname)
    else:
        return "%s" % (tpname)



def size_of_one_run(run):
    return sum([x.size for x in run])

def size_from_offset_map(offset_map, comment_list, offset_of_interest=None):
    tot_size = 0

    if offset_of_interest:
        starts = [offset_of_interest]
    else:
        starts = offset_map.keys()

    starts.sort()
    for i in starts:
        if offset_of_interest and i is not offset_of_interest:
            continue
        elif i != tot_size:
            comment_list.append("// Size vs offset misalignment at offset 0x%x (curr size 0x%x)"
                                % (i, tot_size))
        
        runs_ll = offset_map[i] # list of lists

        # size of union is max of its components
        #tot_size += max ([len(x) for x in runs_ll])
        tot_size += max ([size_of_one_run(x) for x in runs_ll])
        
    return tot_size

def member_list_from_records(records):
    """
    Generate member_list suitable for flstr() from the runs generated in
    unionize(). Attempt to make acceptable formatting.
    """
    return records.to_string(typedef=True).split('\n')
        
def member_list_from_offset_map(offset_map, leaf_type):
    """
    Generate member_list suitable for flstr() from the offset map generated in
    unionize(). Attempt to make acceptable formatting.
    """
    my_mlist = list()

    starts = offset_map.keys()
    starts.sort()

    for ofs in starts: # i is either a scalar or a list
        # runs is a list of lists
        runs = offset_map[ofs]

        # how many items in sublist? 1 ==> emit it, 2+ ==> put in union
        if len(runs) == 1: # one run at this offset
            for member in runs[0]:
                my_mlist.append(str(member))
        else: # multiple runs at this offset
            if leaf_type is not 'LF_UNION':
                union_str = "union {\n"
            else:
                union_str = ""
            sizes = list()
            for member in runs:
                if len(member) == 1:
                    union_str += str(member[0]) +'\n'
                    sizes.append(member[0].size)
                else:
                    struct_size = sum([x.size for x in member])
                    sizes.append(struct_size)
                    
                    struct =  ('struct { // offset 0x%x\n' % member[0].ofs +
                               '\n'.join([str(x) for x in member]) + '\n' +
                               '}; // struct size 0x%x\n' % struct_size )
                    union_str += struct
                    
            if leaf_type is not 'LF_UNION':
                union_str += '};' ### // union size 0x%x' % max(sizes)

            my_mlist.append (union_str)

    return my_mlist

def is_bitfield(member):
    if member is None: 
        return False
    if not isinstance(member,Member):
        return False
    
    return member.leaf_type == 'LF_BITFIELD'

def first_ofs_appears_later(members):
    mbr = members[0]
    for next in members[1:]:
        if next.ofs < mbr.ofs:
            raise ValueError('I need non decreasing offsets runs.')
        elif next.ofs == mbr.ofs:
            return True
    return False

def make_sub_runs(members):
    """Groups sequences of fields using their offsets. Make Struct and Union.""" 
    if len(members) == 1:
        ret = Struct()
        ret.extend(members)
        return ret
    # Returns a list of fields ( Members or list)
    # if a field is an union, the fields will be in a list.
    if first_ofs_appears_later(members):
        # we have a union
        if len(members) == 2:
            ret = Union()
            ret.extend([members[0],members[1]])
            return ret
        # members need to be limited to current group (non-decreasing index)
        runs = Union()
        run = Struct()
        run.append(members[0])
        ofs = members[0].ofs
        for m in members[1:]:
            if m.ofs > ofs: # OneRun
                run.append(m)
            elif m.ofs == ofs:
                # new member/structure of the union
                if len(run) == 1:
                    runs.append(run[0])
                else:
                    runs.append(run)
                run = Struct()
                run.append(m)
            else:
                raise ValueError('I need non decreasing offsets runs.')
        runs.append(run)
        fields = Union()
        # [ lst, lst, lst ]  == field n , lst = structure
        for sub in runs:
            if isinstance(sub, Record):
                sub_fields = make_sub_runs(sub)
            else:
                sub_fields = sub
            fields.append(sub_fields)
    else:
        fields = Struct()
        fields.append(members[0])
        next_fields = make_sub_runs(members[1:])
        # [ f, f, union, f, f]
        if isinstance(next_fields, Union):
            # FIXME check union or union +struct
            next_fields = simplify_union(next_fields)

        if isinstance(next_fields, Union):
            fields.append(next_fields)
        elif len(next_fields) == 1: # lonely field
            fields.append(next_fields[0])
        else: # Structure with fields
            fields.extend(next_fields)
    
    return fields

def simplify_union(union):
    """ if a union field is field n of a structure, fields n+1 have been 
    aggregated in union. Lets cut them out and put them back in the structure.
    """
    # if last structure is bigger, its fields are probably not part of the union
    last_field = union[-1]
    if not isinstance(last_field, Record):
        return union
    # get size of last field
    last_size = last_field.get_size()
    size = 0
    # if last structure is bigger, its fields are probably not part of the union
    for st in union[:-1]:
        size = max(size, st.get_size())
    if last_size <= size: # other fields are bigger.
        # bigger fields means its an single union ( ordered PDB records )
        return union
    # last union field is bigger. try to cut out some sub fields.
    # get union offset.
    ofs = union.ofs
    break_index = 0
    # check if last union field 
    for i,f in enumerate(last_field):
        f_ofs = f.ofs-ofs
        if f_ofs == size:
            # that field ( and following ) is in fact, out of the union.
            break_index = i
            break
    if break_index > 0:
        # recreate the union
        new_union = Union()
        new_union.extend(union[:-1])
        
        # add the new smaller last_field of the union
        if len(last_field[:break_index]) == 1:
            new_last_field = last_field[0]
        else:
            new_last_field = Struct()
            new_last_field.extend(last_field[:break_index])
        new_union.append(new_last_field)

        # change the union into fields ( struct() make_sub will cope with that)
        new_root_fields = Struct()
        new_root_fields.append(new_union)
        if len(last_field[break_index:]) == 1:
            new_root_fields.append(last_field[break_index])
        else:
            new_root_fields.extend(last_field[break_index:])
        return new_root_fields

    return union

def generate_gap_member(ofs, size, gap_ct):
    assert size>0, "invalid size for gap"
    name = 'UINT8 unknown%d[%#x];' % (gap_ct, size)
    return Member(ofs, size, None, name, 0)

def flush_run_to_map(offset_map, run):
    if len(run) > 0:
        if run.ofs not in offset_map:
            offset_map[run.ofs] = list()
        offset_map[run.ofs].append ([m for m in run.members])

def fix_bitfield_offsets(members):
    i = 1
    while i < len(members):
        this = members[i]
        prev = members[i-1]

        if is_bitfield(this) and is_bitfield(prev):
            this.ofs = prev.ofs + prev.size
        i += 1

def merge_bitfield_sequences(members):

    def get_aligned_size (sz, alignment):
        floor_int = int(sz)
        if sz == floor_int:
            sz_int = int(sz)
        else:
            # round up to nearest int
            sz_int = int(sz + 1)


        if sz_int & (alignment-1) == 0:
            aligned = sz_int
        else:
            aligned = int ((sz_int + alignment) / alignment)

        return aligned

    new_members = list()
    in_bitfield = False
    bf_member = None
    
    i = 0
    idx = 0
    while i < len(members):
        curr = members[i]
        
        if is_bitfield(curr):
            if in_bitfield:
                bf_member.contents += str(curr) + '\n'
                bf_member.size     += curr.size
            else: # new bitfield sequence
                in_bitfield = True
                bf_member = Member(curr.ofs, curr.size, curr.alignment,
                                   'LF_STRUCTURE', 'struct { // bitfield(s)\n',
                                   i, suppress_meta=True)
                bf_member.idx = idx
                idx += 1

                bf_member.contents += str(curr) + '\n'
                
        else: # not a bitfield
            if in_bitfield:
                in_bitfield = False
                bf_member.size = get_aligned_size (bf_member.size, bf_member.alignment)
                bf_member.contents += '};'
                new_members.append(bf_member)
                bf_member = None

            new_members.append(curr)
            curr.idx = idx
            idx += 1

        i += 1

    if bf_member is not None:
        bf_member.contents += '};'
        bf_member.size = get_aligned_size (bf_member.size, bf_member.alignment)
        new_members.append(bf_member)
        
    return new_members

class Solution:
    def __init__(self):
        self.computed_size = None
        self.claimed_size  = None
        self.mlist         = list()
        self.comments      = list()

def fill_gaps(lf, members, mbr_ct_by_ofs):
    '''
    Fill in gaps -- areas where there are no members to account for the space.
    '''

    new_mlist = list()
    
    ofs = 0
    while ofs < lf.size:
        try:
            # Find next "bare" spot with 0 members. Raises exception if no
            # such member.
            ofs = mbr_ct_by_ofs.index(0, ofs)

            # HERE: There's a bare spot. -----------------

            # Where does the bare spot end?
            j = ofs+1
            while j < lf.size:
                if mbr_ct_by_ofs[j] != 0:
                    break
                j += 1
                
            # [ofs:j) is bare
            gap_size = j-ofs
            gap_name = ('UINT8 gap_in_pdb_ofs_%X[%#x];' % (ofs, gap_size))

            gap_filler = Member(ofs, gap_size, None, None, gap_name, 0)

            # Insert gap_filler into members at right spot.
            
            mbr_ct_by_ofs[ofs:j] = [1] * gap_size # now there's one member in this range.

            # Calcule the right spot to insert the gap filler.
            k = 0
            members_len = len(members)
            while k < members_len:
                m = members[k]
                k += 1

                if m.ofs > ofs: # m is first member beyond bare spot
                    members.insert (k-1, gap_filler)
                    break
            if k == len(members): # gap is at end
                members.append(gap_filler)
                
            ofs += gap_size
            
        except ValueError:
            break
    
def unionize_compute(lf, member_list):
    if lf.leaf_type == 'LF_ENUM':
        s = Solution()
        return s # empty mlist

    this_run = OneRun(0)
    gap_ct = 0 # 
    
    # maps an offset to a list of runs starting at that offset. Multiple runs
    # at one offset indicate a union, whereas one run indicates non-union
    # member(s).
    offset_map = dict() 
    raw_members = [Member(ofs,size,align,ltype,s,idx)
                   for idx,(ofs,size,align,ltype,s) in enumerate(member_list)]

    fix_bitfield_offsets(raw_members)

    members = merge_bitfield_sequences(raw_members)

    offsets = [x.ofs for x in members]
    #byte_ct = max([x.ofs + x.size for x in members])
    byte_ct = lf.size
    
    # count how many members occupy each offset
    mbr_ct_by_ofs = [0] * byte_ct
    for m in members:
        for ofs in range(m.ofs, m.ofs+m.size):
            mbr_ct_by_ofs[ofs] += 1

    # fix indices
    for i,m in enumerate(members):
        m.index = i

    fill_gaps (lf, members, mbr_ct_by_ofs)

    member_ct = len(members)

    # fix indices
    for i,m in enumerate(members):
        m.index = i

    #if lf.name == '_MM_PAGE_ACCESS_INFO_HEADER':
    #    import pdb;pdb.set_trace()
    
    # order members per group of increasing offset
    records = make_sub_runs(members)

    new_mlist = member_list_from_records(records)

    s = Solution()
    s.computed_size = records.get_size()
    s.claimed_size  = lf.size
    
    if s.computed_size != s.claimed_size:
        s.comments.append ("// ************ INCORRECT SIZE *************************")
        s.comments.append ("// claimed in PDB: 0x%x, calculated: 0x%x"
                           % (s.claimed_size,s.computed_size))

         # e.g. VISTA SP2 x86_32: ntdll.pdb(struct _DISPATCHER_HEADER)
        new_mlist.insert(0, '/*')
        new_mlist.append('*/')
        new_mlist.append('UINT8 blob[0x%x]; // print_ctypes.py validation failure' % lf.size)

    s.mlist = new_mlist
    return s

    
def flstr(lf):
    flstr = ""
    memb_strs = [ memb_str(s.index,s.name,s.offset) for s in lf.fieldlist.substructs
                  if s.leaf_type == "LF_MEMBER" ]

    sol = unionize_compute(lf, memb_strs)

    if sol.comments:
        flstr += '\n'.join(sol.comments) + '\n'
    
    
    if lf.leaf_type in ["LF_STRUCTURE","LF_UNION"]:
        return '\n'.join(sol.mlist)
    
    level = 1 # indentation level
    for i,m in enumerate(sol.mlist):
        #eol = '\n' if i < len(sol.mlist)-1 else ''
        eol = '\n' # figure out bad formatting caused by above line
        
        if isinstance(m,list):
            for um in m:
                sl = um.splitlines()
                for u in sl:
                    lstr = u.lstrip()
                    if '}' in u:
                        level -= 1

                    flstr += indent*(level) + lstr + eol
                    if '{' in u:
                        level += 1
        else:
            sl = m.splitlines()
            for u in sl:

                lstr = u.lstrip()
                if '}' in u:
                    level -= 1

                flstr += indent*(level) + lstr + eol
                if '{' in u:
                    level += 1

    enum_membs = [ e for e in lf.fieldlist.substructs if e.leaf_type == "LF_ENUMERATE" ]
    for i,e in enumerate(enum_membs):
        e_val = -1 if e.enum_value == '\xff' else e.enum_value
        comma = ",\n" if i < len(enum_membs) - 1 else ""
        flstr += '{0}{1:<70} = {2:>4}{3}'.format(indent,e.name, e_val, comma)#indent + "%s = %s%s\n" % (e.name, e_val, comma)
    return flstr

def struct_dependencies(lf):
    deps = set()
    members = [ s for s in lf.fieldlist.substructs if s.leaf_type == "LF_MEMBER" ]
    for memb in members:
        base = get_basetype(memb.index)
        if base and not (memb.index.leaf_type == "LF_POINTER"):
            if is_inline_struct(base):
                deps = deps | struct_dependencies(base)
            else:
                deps.add(base.name)
    return deps

def struct_pretty_str_fwd(lf, gcc):
    print "%s %s { // %#x bytes" % (snames[lf.leaf_type], mangle(lf.name), lf.size)
    print flstr(lf)
    if gcc:
        print "} __attribute__((packed));"
    else:
        print "};"
    print

def struct_pretty_str_nofwd(lf, gcc):
    print "typedef %s %s { // %#x bytes" % (snames[lf.leaf_type], mangle(lf.name), lf.size)
    print flstr(lf)
    if gcc:
        print "} __attribute__((packed)) %s, *P%s, **PP%s ;" % ((demangle(lf.name),)*3)
    else:
        print "} %s, *P%s, **PP%s ;" % ((demangle(lf.name),)*3)
    print

def enum_pretty_str(enum):
    #if not enum.name.startswith("_"):
    #    name = "_" + enum.name
    #else: name = enum.name
    print "typedef enum %s {" % mangle(enum.name)
    print flstr(enum)
    print "} %s;" % demangle(enum.name)
    print

themes = {
    "msvc": ctype_msvc,
    "intro": ctype_intro,
}

theme_func = {
    "msvc": {
        "ptr_str": ptr_str_std,
        "fptr_str": fptr_str_std,
    },
    "intro": {
        "ptr_str": ptr_str_intro,
        "fptr_str": fptr_str_intro,
    },
}

if __name__ == "__main__":
    from optparse import OptionParser
    parser = OptionParser()

    parser.add_option("-g", "--gcc", dest="gcc",
                      help="emit code to assist in compilation under gcc (e.g. \"typedef uint32_t UINT32\")",
                      action="store_true", default=False)
    parser.add_option("-m", "--macroguard", dest="macroguard",
                      help="emit macroguards around output",
                      action="store_true", default=False)
    parser.add_option("-t", "--theme", dest="theme",
                      help="theme to use for C types [%s]" % ", ".join(themes),
                      default="msvc")
    parser.add_option("-f", "--fwdrefs", dest="fwdrefs", action="store_true",
                      help="emit forward references", default=False)
    parser.add_option("-w", "--width", dest="width",
                      help="set pointer width for PDB's target architecture",
                      type="int", default=None)
    
    opts,args = parser.parse_args()
    ctype = themes[opts.theme]
    ptr_str = theme_func[opts.theme]["ptr_str"]
    fptr_str = theme_func[opts.theme]["fptr_str"]
    if opts.fwdrefs:
        struct_pretty_str = struct_pretty_str_fwd
    else:
        struct_pretty_str =  struct_pretty_str_nofwd

    if opts.fwdrefs:
        pdb = pdbparse.parse(args[0], fast_load=True)
        pdb.streams[2].load(elim_fwdrefs=False)
    else:
        pdb = pdbparse.parse(args[0])

    # Determine the pointer width, set global ARCH_PTR_SIZE
    if opts.width:
        ARCH_PTR_SIZE = opts.width
    else:
        # TODO: this causes a ConstError occassionally (can't be reliably
        # reproduced).
        try:
            ##pdb.STREAM_DBI.load()

            # sets global ARCH_PTR_SIZE
            if pdb.STREAM_DBI.machine in ('IMAGE_FILE_MACHINE_I386'):
                print "// Architecture pointer width 4 bytes"
                ARCH_PTR_SIZE = 4
            elif pdb.STREAM_DBI.machine in ('IMAGE_FILE_MACHINE_AMD64',
                                            'IMAGE_FILE_MACHINE_IA64'):
                print "// Architecture pointer width 8 bytes"
                ARCH_PTR_SIZE = 8
            else:
                sys.stderr.write ("Failed to find arch pointer width. Use the -w option.")
                raise

        except:
            sys.stderr.write ("Failed to find arch pointer width. Use the -w option.")
            raise


            
    if opts.macroguard:
        macroguard_str = "_WINDOWS_PDB_" + os.path.basename(args[0]).replace('.', '_') + "_defns"
        print "#ifndef %s" % macroguard_str
        print "#define %s" % macroguard_str
        print
        
    if opts.gcc:
        print_basic_types()
        
    if opts.fwdrefs:
        fwdrefs = [ s for s in pdb.streams[2].types.values()
                    if s.leaf_type in ("LF_STRUCTURE","LF_UNION") and s.prop.fwdref ]
        print "/******* Forward Refs *******/"
        for f in fwdrefs:
            print "%s %s;" % (snames[f.leaf_type], mangle(f.name))
            print "typedef %s %s %s;" % \
                (snames[f.leaf_type], mangle(f.name),demangle(f.name))
            print "#ifdef WINDOWS_USE_32_BIT_POINTERS"
            print "   typedef %s P%s, PP%s; // pointers take up 32 bits" % \
                ("UINT32", demangle(f.name), demangle(f.name))
            print "#else"
            print "   typedef %s *P%s, **PP%s;" % ((demangle(f.name),)*3)
            print "#endif"
            print
        # Reload the file without fwdrefs as it messes up type sizes
        pdb = pdbparse.parse(args[0])

    structs = [ s for s in pdb.streams[2].types.values()
                if (s.leaf_type in ("LF_STRUCTURE","LF_UNION")
                    and not s.prop.fwdref ) ]
    enums = [ e for e in pdb.streams[2].types.values()
              if e.leaf_type == "LF_ENUM" and not e.prop.fwdref ]

    dep_graph = {}
    names = {}
    for s in structs:
        if "unnamed" in s.name: continue
        dep_graph[s.name] = struct_dependencies(s)
        names[s.name] = s
    enums_names = [e.name for e in enums]
    dep_graph.update((e.name,[]) for e in enums)
    sorted_structs = topological_sort(dep_graph)
    sorted_structs.reverse()
    
    #import code 
    #code.interact(local=locals())

    print "/******* Enumerations *******/"
    for e in enums:
        enum_pretty_str(e)

    print "/*******  Structures  *******/"
    for n in sorted_structs:
        if n in enums_names: continue
        s = names[n]
        if "unnamed" in s.name: continue
        if s.leaf_type == "LF_ENUM": continue
        struct_pretty_str(s, opts.gcc)

    if opts.macroguard:
        print "#endif // #define %s" % macroguard_str
