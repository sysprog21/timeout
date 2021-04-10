/*
 * Elastic Binary Trees - generic macros and structures.
 * Version 6.0.6
 * (C) 2002-2011 - Willy Tarreau <w@1wt.eu>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation, version 2.1
 * exclusively.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

/*
  General idea:
  -------------
  In a radix binary tree, we may have up to 2N-1 nodes for N keys if all of
  them are leaves. If we find a way to differentiate intermediate nodes (later
  called "nodes") and final nodes (later called "leaves"), and we associate
  them by two, it is possible to build sort of a self-contained radix tree with
  intermediate nodes always present. It will not be as cheap as the ultree for
  optimal cases as shown below, but the optimal case almost never happens :

  Eg, to store 8, 10, 12, 13, 14 :

             ultree          this theorical tree

               8                   8
              / \                 / \
             10 12               10 12
               /  \                /  \
              13  14              12  14
                                 / \
                                12 13

   Note that on real-world tests (with a scheduler), is was verified that the
   case with data on an intermediate node never happens. This is because the
   data spectrum is too large for such coincidences to happen. It would require
   for instance that a task has its expiration time at an exact second, with
   other tasks sharing that second. This is too rare to try to optimize for it.

   What is interesting is that the node will only be added above the leaf when
   necessary, which implies that it will always remain somewhere above it. So
   both the leaf and the node can share the exact value of the leaf, because
   when going down the node, the bit mask will be applied to comparisons. So we
   are tempted to have one single key shared between the node and the leaf.

   The bit only serves the nodes, and the dups only serve the leaves. So we can
   put a lot of information in common. This results in one single entity with
   two branch pointers and two parent pointers, one for the node part, and one
   for the leaf part :

              node's         leaf's
              parent         parent
                |              |
              [node]         [leaf]
               / \
           left   right
         branch   branch

   The node may very well refer to its leaf counterpart in one of its branches,
   indicating that its own leaf is just below it :

              node's
              parent
                |
              [node]
               / \
           left  [leaf]
         branch

   Adding keys in such a tree simply consists in inserting nodes between
   other nodes and/or leaves :

                [root]
                  |
               [node2]
                 / \
          [leaf1]   [node3]
                      / \
               [leaf2]   [leaf3]

   On this diagram, we notice that [node2] and [leaf2] have been pulled away
   from each other due to the insertion of [node3], just as if there would be
   an elastic between both parts. This elastic-like behaviour gave its name to
   the tree : "Elastic Binary Tree", or "EBtree". The entity which associates a
   node part and a leaf part will be called an "EB node".

   We also notice on the diagram that there is a root entity required to attach
   the tree. It only contains two branches and there is nothing above it. This
   is an "EB root". Some will note that [leaf1] has no [node1]. One property of
   the EBtree is that all nodes have their branches filled, and that if a node
   has only one branch, it does not need to exist. Here, [leaf1] was added
   below [root] and did not need any node.

   An EB node contains :
     - a pointer to the node's parent (node_p)
     - a pointer to the leaf's parent (leaf_p)
     - two branches pointing to lower nodes or leaves (branches)
     - a bit position (bit)
     - an optional key.

   The key here is optional because it's used only during insertion, in order
   to classify the nodes. Nothing else in the tree structure requires knowledge
   of the key. This makes it possible to write type-agnostic primitives for
   everything, and type-specific insertion primitives. This has led to consider
   two types of EB nodes. The type-agnostic ones will serve as a header for the
   other ones, and will simply be called "struct eb_node". The other ones will
   have their type indicated in the structure name. Eg: "struct eb32_node" for
   nodes carrying 32 bit keys.

   We will also node that the two branches in a node serve exactly the same
   purpose as an EB root. For this reason, a "struct eb_root" will be used as
   well inside the struct eb_node. In order to ease pointer manipulation and
   ROOT detection when walking upwards, all the pointers inside an eb_node will
   point to the eb_root part of the referenced EB nodes, relying on the same
   principle as the linked lists in Linux.

   Another important point to note, is that when walking inside a tree, it is
   very convenient to know where a node is attached in its parent, and what
   type of branch it has below it (leaf or node). In order to simplify the
   operations and to speed up the processing, it was decided in this specific
   implementation to use the lowest bit from the pointer to designate the side
   of the upper pointers (left/right) and the type of a branch (leaf/node).
   This practise is not mandatory by design, but an implementation-specific
   optimisation permitted on all platforms on which data must be aligned. All
   known 32 bit platforms align their integers and pointers to 32 bits, leaving
   the two lower bits unused. So, we say that the pointers are "tagged". And
   since they designate pointers to root parts, we simply call them
   "tagged root pointers", or "eb_troot" in the code.

   Duplicate keys are stored in a special manner. When inserting a key, if
   the same one is found, then an incremental binary tree is built at this
   place from these keys. This ensures that no special case has to be written
   to handle duplicates when walking through the tree or when deleting entries.
   It also guarantees that duplicates will be walked in the exact same order
   they were inserted. This is very important when trying to achieve fair
   processing distribution for instance.

   Algorithmic complexity can be derived from 3 variables :
     - the number of possible different keys in the tree : P
     - the number of entries in the tree : N
     - the number of duplicates for one key : D

   Note that this tree is deliberately NOT balanced. For this reason, the worst
   case may happen with a small tree (eg: 32 distinct keys of one bit). BUT,
   the operations required to manage such data are so much cheap that they make
   it worth using it even under such conditions. For instance, a balanced tree
   may require only 6 levels to store those 32 keys when this tree will
   require 32. But if per-level operations are 5 times cheaper, it wins.

   Minimal, Maximal and Average times are specified in number of operations.
   Minimal is given for best condition, Maximal for worst condition, and the
   average is reported for a tree containing random keys. An operation
   generally consists in jumping from one node to the other.

   Complexity :
     - lookup              : min=1, max=log(P), avg=log(N)
     - insertion from root : min=1, max=log(P), avg=log(N)
     - insertion of dups   : min=1, max=log(D), avg=log(D)/2 after lookup
     - deletion            : min=1, max=1,      avg=1
     - prev/next           : min=1, max=log(P), avg=2 :
       N/2 nodes need 1 hop  => 1*N/2
       N/4 nodes need 2 hops => 2*N/4
       N/8 nodes need 3 hops => 3*N/8
       ...
       N/x nodes need log(x) hops => log2(x)*N/x
       Total cost for all N nodes : sum[i=1..N](log2(i)*N/i) =
  N*sum[i=1..N](log2(i)/i) Average cost across N nodes = total / N =
  sum[i=1..N](log2(i)/i) = 2

   This design is currently limited to only two branches per node. Most of the
   tree descent algorithm would be compatible with more branches (eg: 4, to cut
   the height in half), but this would probably require more complex operations
   and the deletion algorithm would be problematic.

   Useful properties :
     - a node is always added above the leaf it is tied to, and never can get
       below nor in another branch. This implies that leaves directly attached
       to the root do not use their node part, which is indicated by a NULL
       value in node_p. This also enhances the cache efficiency when walking
       down the tree, because when the leaf is reached, its node part will
       already have been visited (unless it's the first leaf in the tree).

     - pointers to lower nodes or leaves are stored in "branch" pointers. Only
       the root node may have a NULL in either branch, it is not possible for
       other branches. Since the nodes are attached to the left branch of the
       root, it is not possible to see a NULL left branch when walking up a
       tree. Thus, an empty tree is immediately identified by a NULL left
       branch at the root. Conversely, the one and only way to identify the
       root node is to check that it right branch is NULL. Note that the
       NULL pointer may have a few low-order bits set.

     - a node connected to its own leaf will have branch[0|1] pointing to
       itself, and leaf_p pointing to itself.

     - a node can never have node_p pointing to itself.

     - a node is linked in a tree if and only if it has a non-null leaf_p.

     - a node can never have both branches equal, except for the root which can
       have them both NULL.

     - deletion only applies to leaves. When a leaf is deleted, its parent must
       be released too (unless it's the root), and its sibling must attach to
       the grand-parent, replacing the parent. Also, when a leaf is deleted,
       the node tied to this leaf will be removed and must be released too. If
       this node is different from the leaf's parent, the freshly released
       leaf's parent will be used to replace the node which must go. A released
       node will never be used anymore, so there's no point in tracking it.

     - the bit index in a node indicates the bit position in the key which is
       represented by the branches. That means that a node with (bit == 0) is
       just above two leaves. Negative bit values are used to build a duplicate
       tree. The first node above two identical leaves gets (bit == -1). This
       value logarithmically decreases as the duplicate tree grows. During
       duplicate insertion, a node is inserted above the highest bit value (the
       lowest absolute value) in the tree during the right-sided walk. If bit
       -1 is not encountered (highest < -1), we insert above last leaf.
       Otherwise, we insert above the node with the highest value which was not
       equal to the one of its parent + 1.

     - the "eb_next" primitive walks from left to right, which means from lower
       to higher keys. It returns duplicates in the order they were inserted.
       The "eb_first" primitive returns the left-most entry.

     - the "eb_prev" primitive walks from right to left, which means from
       higher to lower keys. It returns duplicates in the opposite order they
       were inserted. The "eb_last" primitive returns the right-most entry.

     - a tree which has 1 in the lower bit of its root's right branch is a
       tree with unique nodes. This means that when a node is inserted with
       a key which already exists will not be inserted, and the previous
       entry will be returned.

 */

#include <stdlib.h>

/* Support passing function parameters in registers. For this, the
 * CONFIG_REGPARM macro has to be set to the maximal number of registers
 * allowed. Some functions have intentionally received a regparm lower than
 * their parameter count, it is in order to avoid register clobbering where
 * they are called.
 */
#ifndef REGPRM1
#if CONFIG_REGPARM >= 1 && __GNUC__ >= 3
#define REGPRM1 __attribute__((regparm(1)))
#else
#define REGPRM1
#endif
#endif

#ifndef REGPRM2
#if CONFIG_REGPARM >= 2 && __GNUC__ >= 3
#define REGPRM2 __attribute__((regparm(2)))
#else
#define REGPRM2 REGPRM1
#endif
#endif

/* By default, gcc does not inline large chunks of code, but we want it to
 * respect our choices.
 */
#if !defined(forceinline)
#if __GNUC__ < 3
#define forceinline inline
#else
#define forceinline inline __attribute__((always_inline))
#endif
#endif

/*
 * Gcc >= 3 provides the ability for the programme to give hints to the
 * compiler about what branch of an if is most likely to be taken. This
 * helps the compiler produce the most compact critical paths, which is
 * generally better for the cache and to reduce the number of jumps.
 */
#if !defined(likely)
#if __GNUC__ < 3
#define __builtin_expect(x, y) (x)
#define likely(x) (x)
#define unlikely(x) (x)
#elif __GNUC__ < 4
/* gcc 3.x does the best job at this */
#define likely(x) (__builtin_expect((x) != 0, 1))
#define unlikely(x) (__builtin_expect((x) != 0, 0))
#else
/* GCC 4.x is stupid, it performs the comparison then compares it to 1,
 * so we cheat in a dirty way to prevent it from doing this. This will
 * only work with ints and booleans though.
 */
#define likely(x) (x)
#define unlikely(x) (__builtin_expect((unsigned long) (x), 0))
#endif
#endif

static inline int flsnz8_generic(unsigned int x)
{
    int ret = 0;
    if (x >> 4) {
        x >>= 4;
        ret += 4;
    }
    return ret + ((0xFFFFAA50U >> (x << 1)) & 3) + 1;
}

/* Note: we never need to run fls on null keys, so we can optimize the fls
 * function by removing a conditional jump.
 */
#if defined(__i386__) || defined(__x86_64__)
/* this code is similar on 32 and 64 bit */
static inline int flsnz(int x)
{
    int r;
    __asm__("bsrl %1,%0\n" : "=r"(r) : "rm"(x));
    return r + 1;
}

static inline int flsnz8(unsigned char x)
{
    int r;
    __asm__(
        "movzbl %%al, %%eax\n"
        "bsrl %%eax,%0\n"
        : "=r"(r)
        : "a"(x));
    return r + 1;
}

#else
// returns 1 to 32 for 1<<0 to 1<<31. Undefined for 0.
#define flsnz(___a)                     \
    ({                                  \
        register int ___x, ___bits = 0; \
        ___x = (___a);                  \
        if (___x & 0xffff0000) {        \
            ___x &= 0xffff0000;         \
            ___bits += 16;              \
        }                               \
        if (___x & 0xff00ff00) {        \
            ___x &= 0xff00ff00;         \
            ___bits += 8;               \
        }                               \
        if (___x & 0xf0f0f0f0) {        \
            ___x &= 0xf0f0f0f0;         \
            ___bits += 4;               \
        }                               \
        if (___x & 0xcccccccc) {        \
            ___x &= 0xcccccccc;         \
            ___bits += 2;               \
        }                               \
        if (___x & 0xaaaaaaaa) {        \
            ___x &= 0xaaaaaaaa;         \
            ___bits += 1;               \
        }                               \
        ___bits + 1;                    \
    })

static inline int flsnz8(unsigned int x)
{
    return flsnz8_generic(x);
}
#endif

static inline int fls64(unsigned long long x)
{
    unsigned int h;
    unsigned int bits = 32;

    h = x >> 32;
    if (!h) {
        h = x;
        bits = 0;
    }
    return flsnz(h) + bits;
}

#define fls_auto(x) ((sizeof(x) > 4) ? fls64(x) : flsnz(x))

/* Linux-like "container_of". It returns a pointer to the structure of type
 * <type> which has its member <name> stored at address <ptr>.
 */
#ifndef container_of
#define container_of(ptr, type, name) \
    ((type *) (((char *) (ptr)) - ((long) &((type *) 0)->name)))
#endif

/* returns a pointer to the structure of type <type> which has its member <name>
 * stored at address <ptr>, unless <ptr> is 0, in which case 0 is returned.
 */
#ifndef container_of_safe
#define container_of_safe(ptr, type, name)                                \
    ({                                                                    \
        void *__p = (ptr);                                                \
        __p ? (type *) (__p - ((long) &((type *) 0)->name)) : (type *) 0; \
    })
#endif

/* Number of bits per node, and number of leaves per node */
#define EB_NODE_BITS 1
#define EB_NODE_BRANCHES (1 << EB_NODE_BITS)
#define EB_NODE_BRANCH_MASK (EB_NODE_BRANCHES - 1)

/* Be careful not to tweak those values. The walking code is optimized for NULL
 * detection on the assumption that the following values are intact.
 */
#define EB_LEFT 0
#define EB_RGHT 1
#define EB_LEAF 0
#define EB_NODE 1

/* Tags to set in root->b[EB_RGHT] :
 * - EB_NORMAL is a normal tree which stores duplicate keys.
 * - EB_UNIQUE is a tree which stores unique keys.
 */
#define EB_NORMAL 0
#define EB_UNIQUE 1

/* This is the same as an eb_node pointer, except that the lower bit embeds
 * a tag. See eb_dotag()/eb_untag()/eb_gettag(). This tag has two meanings :
 *  - 0=left, 1=right to designate the parent's branch for leaf_p/node_p
 *  - 0=link, 1=leaf  to designate the branch's type for branch[]
 */
typedef void eb_troot_t;

/* The eb_root connects the node which contains it, to two nodes below it, one
 * of which may be the same node. At the top of the tree, we use an eb_root
 * too, which always has its right branch NULL (+/1 low-order bits).
 */
struct eb_root {
    eb_troot_t *b[EB_NODE_BRANCHES]; /* left and right branches */
};

/* The eb_node contains the two parts, one for the leaf, which always exists,
 * and one for the node, which remains unused in the very first node inserted
 * into the tree. This structure is 20 bytes per node on 32-bit machines. Do
 * not change the order, benchmarks have shown that it's optimal this way.
 */
struct eb_node {
    struct eb_root branches; /* branches, must be at the beginning */
    eb_troot_t *node_p;      /* link node's parent */
    eb_troot_t *leaf_p;      /* leaf node's parent */
    short int bit;           /* link's bit position. */
    short unsigned int pfx;  /* data prefix length, always related to leaf */
};

/* Return the structure of type <type> whose member <member> points to <ptr> */
#define eb_entry(ptr, type, member) container_of(ptr, type, member)

/* The root of a tree is an eb_root initialized with both pointers NULL.
 * During its life, only the left pointer will change. The right one will
 * always remain NULL, which is the way we detect it.
 */
#define EB_ROOT \
    (struct eb_root) { .b = {[0] = NULL, [1] = NULL}, }

#define EB_ROOT_UNIQUE \
    (struct eb_root) { .b = {[0] = NULL, [1] = (void *) 1}, }

#define EB_TREE_HEAD(name) struct eb_root name = EB_ROOT


/***************************************\
 * Private functions. Not for end-user *
\***************************************/

/* Converts a root pointer to its equivalent eb_troot_t pointer,
 * ready to be stored in ->branch[], leaf_p or node_p. NULL is not
 * conserved. To be used with EB_LEAF, EB_NODE, EB_LEFT or EB_RGHT in <tag>.
 */
static inline eb_troot_t *eb_dotag(const struct eb_root *root, const int tag)
{
    return (eb_troot_t *) ((char *) root + tag);
}

/* Converts an eb_troot_t pointer pointer to its equivalent eb_root pointer,
 * for use with pointers from ->branch[], leaf_p or node_p. NULL is conserved
 * as long as the tree is not corrupted. To be used with EB_LEAF, EB_NODE,
 * EB_LEFT or EB_RGHT in <tag>.
 */
static inline struct eb_root *eb_untag(const eb_troot_t *troot, const int tag)
{
    return (struct eb_root *) ((char *) troot - tag);
}

/* returns the tag associated with an eb_troot_t pointer */
static inline int eb_gettag(eb_troot_t *troot)
{
    return (unsigned long) troot & 1;
}

/* Converts a root pointer to its equivalent eb_troot_t pointer and clears the
 * tag, no matter what its value was.
 */
static inline struct eb_root *eb_clrtag(const eb_troot_t *troot)
{
    return (struct eb_root *) ((unsigned long) troot & ~1UL);
}

/* Returns a pointer to the eb_node holding <root> */
static inline struct eb_node *eb_root_to_node(struct eb_root *root)
{
    return container_of(root, struct eb_node, branches);
}

/* Walks down starting at root pointer <start>, and always walking on side
 * <side>. It either returns the node hosting the first leaf on that side,
 * or NULL if no leaf is found. <start> may either be NULL or a branch pointer.
 * The pointer to the leaf (or NULL) is returned.
 */
static inline struct eb_node *eb_walk_down(eb_troot_t *start, unsigned int side)
{
    /* A NULL pointer on an empty tree root will be returned as-is */
    while (eb_gettag(start) == EB_NODE)
        start = (eb_untag(start, EB_NODE))->b[side];
    /* NULL is left untouched (root==eb_node, EB_LEAF==0) */
    return eb_root_to_node(eb_untag(start, EB_LEAF));
}

/* This function is used to build a tree of duplicates by adding a new node to
 * a subtree of at least 2 entries. It will probably never be needed inlined,
 * and it is not for end-user.
 */
static forceinline struct eb_node *__eb_insert_dup(struct eb_node *sub,
                                                   struct eb_node *new)
{
    struct eb_node *head = sub;

    eb_troot_t *new_left = eb_dotag(&new->branches, EB_LEFT);
    eb_troot_t *new_rght = eb_dotag(&new->branches, EB_RGHT);
    eb_troot_t *new_leaf = eb_dotag(&new->branches, EB_LEAF);

    /* first, identify the deepest hole on the right branch */
    while (eb_gettag(head->branches.b[EB_RGHT]) != EB_LEAF) {
        struct eb_node *last = head;
        head = container_of(eb_untag(head->branches.b[EB_RGHT], EB_NODE),
                            struct eb_node, branches);
        if (head->bit > last->bit + 1)
            sub = head; /* there's a hole here */
    }

    /* Here we have a leaf attached to (head)->b[EB_RGHT] */
    if (head->bit < -1) {
        /* A hole exists just before the leaf, we insert there */
        new->bit = -1;
        sub = container_of(eb_untag(head->branches.b[EB_RGHT], EB_LEAF),
                           struct eb_node, branches);
        head->branches.b[EB_RGHT] = eb_dotag(&new->branches, EB_NODE);

        new->node_p = sub->leaf_p;
        new->leaf_p = new_rght;
        sub->leaf_p = new_left;
        new->branches.b[EB_LEFT] = eb_dotag(&sub->branches, EB_LEAF);
        new->branches.b[EB_RGHT] = new_leaf;
        return new;
    } else {
        int side;
        /* No hole was found before a leaf. We have to insert above
         * <sub>. Note that we cannot be certain that <sub> is attached
         * to the right of its parent, as this is only true if <sub>
         * is inside the dup tree, not at the head.
         */
        new->bit = sub->bit - 1; /* install at the lowest level */
        side = eb_gettag(sub->node_p);
        head =
            container_of(eb_untag(sub->node_p, side), struct eb_node, branches);
        head->branches.b[side] = eb_dotag(&new->branches, EB_NODE);

        new->node_p = sub->node_p;
        new->leaf_p = new_rght;
        sub->node_p = new_left;
        new->branches.b[EB_LEFT] = eb_dotag(&sub->branches, EB_NODE);
        new->branches.b[EB_RGHT] = new_leaf;
        return new;
    }
}


/**************************************\
 * Public functions, for the end-user *
\**************************************/

/* Return non-zero if the tree is empty, otherwise zero */
static inline int eb_is_empty(struct eb_root *root)
{
    return !root->b[EB_LEFT];
}

/* Return non-zero if the node is a duplicate, otherwise zero */
static inline int eb_is_dup(struct eb_node *node)
{
    return node->bit < 0;
}

/* Return the first leaf in the tree starting at <root>, or NULL if none */
static inline struct eb_node *eb_first(struct eb_root *root)
{
    return eb_walk_down(root->b[0], EB_LEFT);
}

/* Return the last leaf in the tree starting at <root>, or NULL if none */
static inline struct eb_node *eb_last(struct eb_root *root)
{
    return eb_walk_down(root->b[0], EB_RGHT);
}

/* Return previous leaf node before an existing leaf node, or NULL if none. */
static inline struct eb_node *eb_prev(struct eb_node *node)
{
    eb_troot_t *t = node->leaf_p;

    while (eb_gettag(t) == EB_LEFT) {
        /* Walking up from left branch. We must ensure that we never
         * walk beyond root.
         */
        if (unlikely(eb_clrtag((eb_untag(t, EB_LEFT))->b[EB_RGHT]) == NULL))
            return NULL;
        t = (eb_root_to_node(eb_untag(t, EB_LEFT)))->node_p;
    }
    /* Note that <t> cannot be NULL at this stage */
    t = (eb_untag(t, EB_RGHT))->b[EB_LEFT];
    return eb_walk_down(t, EB_RGHT);
}

/* Return next leaf node after an existing leaf node, or NULL if none. */
static inline struct eb_node *eb_next(struct eb_node *node)
{
    eb_troot_t *t = node->leaf_p;

    while (eb_gettag(t) != EB_LEFT)
        /* Walking up from right branch, so we cannot be below root */
        t = (eb_root_to_node(eb_untag(t, EB_RGHT)))->node_p;

    /* Note that <t> cannot be NULL at this stage */
    t = (eb_untag(t, EB_LEFT))->b[EB_RGHT];
    if (eb_clrtag(t) == NULL)
        return NULL;
    return eb_walk_down(t, EB_LEFT);
}

/* Return previous leaf node within a duplicate sub-tree, or NULL if none. */
static inline struct eb_node *eb_prev_dup(struct eb_node *node)
{
    eb_troot_t *t = node->leaf_p;

    while (eb_gettag(t) == EB_LEFT) {
        /* Walking up from left branch. We must ensure that we never
         * walk beyond root.
         */
        if (unlikely(eb_clrtag((eb_untag(t, EB_LEFT))->b[EB_RGHT]) == NULL))
            return NULL;
        /* if the current node leaves a dup tree, quit */
        if ((eb_root_to_node(eb_untag(t, EB_LEFT)))->bit >= 0)
            return NULL;
        t = (eb_root_to_node(eb_untag(t, EB_LEFT)))->node_p;
    }
    /* Note that <t> cannot be NULL at this stage */
    if ((eb_root_to_node(eb_untag(t, EB_RGHT)))->bit >= 0)
        return NULL;
    t = (eb_untag(t, EB_RGHT))->b[EB_LEFT];
    return eb_walk_down(t, EB_RGHT);
}

/* Return next leaf node within a duplicate sub-tree, or NULL if none. */
static inline struct eb_node *eb_next_dup(struct eb_node *node)
{
    eb_troot_t *t = node->leaf_p;

    while (eb_gettag(t) != EB_LEFT) {
        /* Walking up from right branch, so we cannot be below root */
        /* if the current node leaves a dup tree, quit */
        if ((eb_root_to_node(eb_untag(t, EB_RGHT)))->bit >= 0)
            return NULL;
        t = (eb_root_to_node(eb_untag(t, EB_RGHT)))->node_p;
    }

    /* Note that <t> cannot be NULL at this stage */
    if ((eb_root_to_node(eb_untag(t, EB_LEFT)))->bit >= 0)
        return NULL;
    t = (eb_untag(t, EB_LEFT))->b[EB_RGHT];
    if (eb_clrtag(t) == NULL)
        return NULL;
    return eb_walk_down(t, EB_LEFT);
}

/* Return previous leaf node before an existing leaf node, skipping duplicates,
 * or NULL if none. */
static inline struct eb_node *eb_prev_unique(struct eb_node *node)
{
    eb_troot_t *t = node->leaf_p;

    while (1) {
        if (eb_gettag(t) != EB_LEFT) {
            node = eb_root_to_node(eb_untag(t, EB_RGHT));
            /* if we're right and not in duplicates, stop here */
            if (node->bit >= 0)
                break;
            t = node->node_p;
        } else {
            /* Walking up from left branch. We must ensure that we never
             * walk beyond root.
             */
            if (unlikely(eb_clrtag((eb_untag(t, EB_LEFT))->b[EB_RGHT]) == NULL))
                return NULL;
            t = (eb_root_to_node(eb_untag(t, EB_LEFT)))->node_p;
        }
    }
    /* Note that <t> cannot be NULL at this stage */
    t = (eb_untag(t, EB_RGHT))->b[EB_LEFT];
    return eb_walk_down(t, EB_RGHT);
}

/* Return next leaf node after an existing leaf node, skipping duplicates, or
 * NULL if none.
 */
static inline struct eb_node *eb_next_unique(struct eb_node *node)
{
    eb_troot_t *t = node->leaf_p;

    while (1) {
        if (eb_gettag(t) == EB_LEFT) {
            if (unlikely(eb_clrtag((eb_untag(t, EB_LEFT))->b[EB_RGHT]) == NULL))
                return NULL; /* we reached root */
            node = eb_root_to_node(eb_untag(t, EB_LEFT));
            /* if we're left and not in duplicates, stop here */
            if (node->bit >= 0)
                break;
            t = node->node_p;
        } else {
            /* Walking up from right branch, so we cannot be below root */
            t = (eb_root_to_node(eb_untag(t, EB_RGHT)))->node_p;
        }
    }

    /* Note that <t> cannot be NULL at this stage */
    t = (eb_untag(t, EB_LEFT))->b[EB_RGHT];
    if (eb_clrtag(t) == NULL)
        return NULL;
    return eb_walk_down(t, EB_LEFT);
}


/* Removes a leaf node from the tree if it was still in it. Marks the node
 * as unlinked.
 */
static forceinline void __eb_delete(struct eb_node *node)
{
    __label__ delete_unlink;
    unsigned int pside, gpside, sibtype;
    struct eb_node *parent;
    struct eb_root *gparent;

    if (!node->leaf_p)
        return;

    /* we need the parent, our side, and the grand parent */
    pside = eb_gettag(node->leaf_p);
    parent = eb_root_to_node(eb_untag(node->leaf_p, pside));

    /* We likely have to release the parent link, unless it's the root,
     * in which case we only set our branch to NULL. Note that we can
     * only be attached to the root by its left branch.
     */

    if (eb_clrtag(parent->branches.b[EB_RGHT]) == NULL) {
        /* we're just below the root, it's trivial. */
        parent->branches.b[EB_LEFT] = NULL;
        goto delete_unlink;
    }

    /* To release our parent, we have to identify our sibling, and reparent
     * it directly to/from the grand parent. Note that the sibling can
     * either be a link or a leaf.
     */

    gpside = eb_gettag(parent->node_p);
    gparent = eb_untag(parent->node_p, gpside);

    gparent->b[gpside] = parent->branches.b[!pside];
    sibtype = eb_gettag(gparent->b[gpside]);

    if (sibtype == EB_LEAF) {
        eb_root_to_node(eb_untag(gparent->b[gpside], EB_LEAF))->leaf_p =
            eb_dotag(gparent, gpside);
    } else {
        eb_root_to_node(eb_untag(gparent->b[gpside], EB_NODE))->node_p =
            eb_dotag(gparent, gpside);
    }
    /* Mark the parent unused. Note that we do not check if the parent is
     * our own node, but that's not a problem because if it is, it will be
     * marked unused at the same time, which we'll use below to know we can
     * safely remove it.
     */
    parent->node_p = NULL;

    /* The parent node has been detached, and is currently unused. It may
     * belong to another node, so we cannot remove it that way. Also, our
     * own node part might still be used. so we can use this spare node
     * to replace ours if needed.
     */

    /* If our link part is unused, we can safely exit now */
    if (!node->node_p)
        goto delete_unlink;

    /* From now on, <node> and <parent> are necessarily different, and the
     * <node>'s node part is in use. By definition, <parent> is at least
     * below <node>, so keeping its key for the bit string is OK.
     */

    parent->node_p = node->node_p;
    parent->branches = node->branches;
    parent->bit = node->bit;

    /* We must now update the new node's parent... */
    gpside = eb_gettag(parent->node_p);
    gparent = eb_untag(parent->node_p, gpside);
    gparent->b[gpside] = eb_dotag(&parent->branches, EB_NODE);

    /* ... and its branches */
    for (pside = 0; pside <= 1; pside++) {
        if (eb_gettag(parent->branches.b[pside]) == EB_NODE) {
            eb_root_to_node(eb_untag(parent->branches.b[pside], EB_NODE))
                ->node_p = eb_dotag(&parent->branches, pside);
        } else {
            eb_root_to_node(eb_untag(parent->branches.b[pside], EB_LEAF))
                ->leaf_p = eb_dotag(&parent->branches, pside);
        }
    }
delete_unlink:
    /* Now the node has been completely unlinked */
    node->leaf_p = NULL;
    return; /* tree is not empty yet */
}

/* Compare blocks <a> and <b> byte-to-byte, from bit <ignore> to bit <len-1>.
 * Return the number of equal bits between strings, assuming that the first
 * <ignore> bits are already identical. It is possible to return slightly more
 * than <len> bits if <len> does not stop on a byte boundary and we find exact
 * bytes. Note that parts or all of <ignore> bits may be rechecked. It is only
 * passed here as a hint to speed up the check.
 */
static forceinline int equal_bits(const unsigned char *a,
                                  const unsigned char *b,
                                  int ignore,
                                  int len)
{
    for (ignore >>= 3, a += ignore, b += ignore, ignore <<= 3; ignore < len;) {
        unsigned char c;

        a++;
        b++;
        ignore += 8;
        c = b[-1] ^ a[-1];

        if (c) {
            /* OK now we know that old and new differ at byte <ptr> and that <c>
             * holds the bit differences. We have to find what bit is differing
             * and report it as the number of identical bits. Note that low bit
             * numbers are assigned to high positions in the byte, as we compare
             * them as strings.
             */
            ignore -= flsnz8(c);
            break;
        }
    }
    return ignore;
}

/* check that the two blocks <a> and <b> are equal on <len> bits. If it is known
 * they already are on some bytes, this number of equal bytes to be skipped may
 * be passed in <skip>. It returns 0 if they match, otherwise non-zero.
 */
static forceinline int check_bits(const unsigned char *a,
                                  const unsigned char *b,
                                  int skip,
                                  int len)
{
    int bit, ret;

    /* This uncommon construction gives the best performance on x86 because
     * it makes heavy use multiple-index addressing and parallel instructions,
     * and it prevents gcc from reordering the loop since it is already
     * properly oriented. Tested to be fine with 2.95 to 4.2.
     */
    bit = ~len + (skip << 3) + 9;  // = (skip << 3) + (8 - len)
    ret = a[skip] ^ b[skip];
    if (unlikely(bit >= 0))
        return ret >> bit;
    while (1) {
        skip++;
        if (ret)
            return ret;
        ret = a[skip] ^ b[skip];
        bit += 8;
        if (bit >= 0)
            return ret >> bit;
    }
}


/* Compare strings <a> and <b> byte-to-byte, from bit <ignore> to the last 0.
 * Return the number of equal bits between strings, assuming that the first
 * <ignore> bits are already identical. Note that parts or all of <ignore> bits
 * may be rechecked. It is only passed here as a hint to speed up the check.
 * The caller is responsible for not passing an <ignore> value larger than any
 * of the two strings. However, referencing any bit from the trailing zero is
 * permitted. Equal strings are reported as a negative number of bits, which
 * indicates the end was reached.
 */
static forceinline int string_equal_bits(const unsigned char *a,
                                         const unsigned char *b,
                                         int ignore)
{
    int beg;
    unsigned char c;

    beg = ignore >> 3;

    /* skip known and identical bits. We stop at the first different byte
     * or at the first zero we encounter on either side.
     */
    while (1) {
        unsigned char d;

        c = a[beg];
        d = b[beg];
        beg++;

        c ^= d;
        if (c)
            break;
        if (!d)
            return -1;
    }
    /* OK now we know that a and b differ at byte <beg>, or that both are zero.
     * We have to find what bit is differing and report it as the number of
     * identical bits. Note that low bit numbers are assigned to high positions
     * in the byte, as we compare them as strings.
     */
    return (beg << 3) - flsnz8(c);
}

static forceinline int cmp_bits(const unsigned char *a,
                                const unsigned char *b,
                                unsigned int pos)
{
    unsigned int ofs;
    unsigned char bit_a, bit_b;

    ofs = pos >> 3;
    pos = ~pos & 7;

    bit_a = (a[ofs] >> pos) & 1;
    bit_b = (b[ofs] >> pos) & 1;

    return bit_a - bit_b; /* -1: a<b; 0: a=b; 1: a>b */
}

static forceinline int get_bit(const unsigned char *a, unsigned int pos)
{
    unsigned int ofs;

    ofs = pos >> 3;
    pos = ~pos & 7;
    return (a[ofs] >> pos) & 1;
}

/* These functions are declared in ebtree.c */
void eb_delete(struct eb_node *node);
REGPRM1 struct eb_node *eb_insert_dup(struct eb_node *sub, struct eb_node *new);

/* Return the structure of type <type> whose member <member> points to <ptr> */
#define eb32_entry(ptr, type, member) container_of(ptr, type, member)

#define EB32_ROOT EB_ROOT
#define EB32_TREE_HEAD EB_TREE_HEAD

/* These types may sometimes already be defined */
typedef unsigned int u32;
typedef signed int s32;

/* This structure carries a node, a leaf, and a key. It must start with the
 * eb_node so that it can be cast into an eb_node. We could also have put some
 * sort of transparent union here to reduce the indirection level, but the fact
 * is, the end user is not meant to manipulate internals, so this is pointless.
 */
struct eb32_node {
    struct eb_node node; /* the tree node, must be at the beginning */
    u32 key;
};

/*
 * Exported functions and macros.
 * Many of them are always inlined because they are extremely small, and
 * are generally called at most once or twice in a program.
 */

/* Return leftmost node in the tree, or NULL if none */
static inline struct eb32_node *eb32_first(struct eb_root *root)
{
    return eb32_entry(eb_first(root), struct eb32_node, node);
}

/* Return rightmost node in the tree, or NULL if none */
static inline struct eb32_node *eb32_last(struct eb_root *root)
{
    return eb32_entry(eb_last(root), struct eb32_node, node);
}

/* Return next node in the tree, or NULL if none */
static inline struct eb32_node *eb32_next(struct eb32_node *eb32)
{
    return eb32_entry(eb_next(&eb32->node), struct eb32_node, node);
}

/* Return previous node in the tree, or NULL if none */
static inline struct eb32_node *eb32_prev(struct eb32_node *eb32)
{
    return eb32_entry(eb_prev(&eb32->node), struct eb32_node, node);
}

/* Return next leaf node within a duplicate sub-tree, or NULL if none. */
static inline struct eb32_node *eb32_next_dup(struct eb32_node *eb32)
{
    return eb32_entry(eb_next_dup(&eb32->node), struct eb32_node, node);
}

/* Return previous leaf node within a duplicate sub-tree, or NULL if none. */
static inline struct eb32_node *eb32_prev_dup(struct eb32_node *eb32)
{
    return eb32_entry(eb_prev_dup(&eb32->node), struct eb32_node, node);
}

/* Return next node in the tree, skipping duplicates, or NULL if none */
static inline struct eb32_node *eb32_next_unique(struct eb32_node *eb32)
{
    return eb32_entry(eb_next_unique(&eb32->node), struct eb32_node, node);
}

/* Return previous node in the tree, skipping duplicates, or NULL if none */
static inline struct eb32_node *eb32_prev_unique(struct eb32_node *eb32)
{
    return eb32_entry(eb_prev_unique(&eb32->node), struct eb32_node, node);
}

/* Delete node from the tree if it was linked in. Mark the node unused. Note
 * that this function relies on a non-inlined generic function: eb_delete.
 */
static inline void eb32_delete(struct eb32_node *eb32)
{
    eb_delete(&eb32->node);
}

/*
 * The following functions are not inlined by default. They are declared
 * in eb32tree.c, which simply relies on their inline version.
 */
REGPRM2 struct eb32_node *eb32_lookup(struct eb_root *root, u32 x);
REGPRM2 struct eb32_node *eb32i_lookup(struct eb_root *root, s32 x);
REGPRM2 struct eb32_node *eb32_lookup_le(struct eb_root *root, u32 x);
REGPRM2 struct eb32_node *eb32_lookup_ge(struct eb_root *root, u32 x);
REGPRM2 struct eb32_node *eb32_insert(struct eb_root *root,
                                      struct eb32_node *new);
REGPRM2 struct eb32_node *eb32i_insert(struct eb_root *root,
                                       struct eb32_node *new);

/*
 * The following functions are less likely to be used directly, because their
 * code is larger. The non-inlined version is preferred.
 */

/* Delete node from the tree if it was linked in. Mark the node unused. */
static forceinline void __eb32_delete(struct eb32_node *eb32)
{
    __eb_delete(&eb32->node);
}

/*
 * Find the first occurence of a key in the tree <root>. If none can be
 * found, return NULL.
 */
static forceinline struct eb32_node *__eb32_lookup(struct eb_root *root, u32 x)
{
    struct eb32_node *node;
    eb_troot_t *troot;
    u32 y;
    int node_bit;

    troot = root->b[EB_LEFT];
    if (unlikely(troot == NULL))
        return NULL;

    while (1) {
        if ((eb_gettag(troot) == EB_LEAF)) {
            node = container_of(eb_untag(troot, EB_LEAF), struct eb32_node,
                                node.branches);
            if (node->key == x)
                return node;
            else
                return NULL;
        }
        node = container_of(eb_untag(troot, EB_NODE), struct eb32_node,
                            node.branches);
        node_bit = node->node.bit;

        y = node->key ^ x;
        if (!y) {
            /* Either we found the node which holds the key, or
             * we have a dup tree. In the later case, we have to
             * walk it down left to get the first entry.
             */
            if (node_bit < 0) {
                troot = node->node.branches.b[EB_LEFT];
                while (eb_gettag(troot) != EB_LEAF)
                    troot = (eb_untag(troot, EB_NODE))->b[EB_LEFT];
                node = container_of(eb_untag(troot, EB_LEAF), struct eb32_node,
                                    node.branches);
            }
            return node;
        }

        if ((y >> node_bit) >= EB_NODE_BRANCHES)
            return NULL; /* no more common bits */

        troot = node->node.branches.b[(x >> node_bit) & EB_NODE_BRANCH_MASK];
    }
}

/*
 * Find the first occurence of a signed key in the tree <root>. If none can
 * be found, return NULL.
 */
static forceinline struct eb32_node *__eb32i_lookup(struct eb_root *root, s32 x)
{
    struct eb32_node *node;
    eb_troot_t *troot;
    u32 key = x ^ 0x80000000;
    u32 y;
    int node_bit;

    troot = root->b[EB_LEFT];
    if (unlikely(troot == NULL))
        return NULL;

    while (1) {
        if ((eb_gettag(troot) == EB_LEAF)) {
            node = container_of(eb_untag(troot, EB_LEAF), struct eb32_node,
                                node.branches);
            if (node->key == (u32) x)
                return node;
            else
                return NULL;
        }
        node = container_of(eb_untag(troot, EB_NODE), struct eb32_node,
                            node.branches);
        node_bit = node->node.bit;

        y = node->key ^ x;
        if (!y) {
            /* Either we found the node which holds the key, or
             * we have a dup tree. In the later case, we have to
             * walk it down left to get the first entry.
             */
            if (node_bit < 0) {
                troot = node->node.branches.b[EB_LEFT];
                while (eb_gettag(troot) != EB_LEAF)
                    troot = (eb_untag(troot, EB_NODE))->b[EB_LEFT];
                node = container_of(eb_untag(troot, EB_LEAF), struct eb32_node,
                                    node.branches);
            }
            return node;
        }

        if ((y >> node_bit) >= EB_NODE_BRANCHES)
            return NULL; /* no more common bits */

        troot = node->node.branches.b[(key >> node_bit) & EB_NODE_BRANCH_MASK];
    }
}

/* Insert eb32_node <new> into subtree starting at node root <root>.
 * Only new->key needs be set with the key. The eb32_node is returned.
 * If root->b[EB_RGHT]==1, the tree may only contain unique keys.
 */
static forceinline struct eb32_node *__eb32_insert(struct eb_root *root,
                                                   struct eb32_node *new)
{
    struct eb32_node *old;
    unsigned int side;
    eb_troot_t *troot, **up_ptr;
    u32 newkey; /* caching the key saves approximately one cycle */
    eb_troot_t *root_right;
    eb_troot_t *new_left, *new_rght;
    eb_troot_t *new_leaf;
    int old_node_bit;

    side = EB_LEFT;
    troot = root->b[EB_LEFT];
    root_right = root->b[EB_RGHT];
    if (unlikely(troot == NULL)) {
        /* Tree is empty, insert the leaf part below the left branch */
        root->b[EB_LEFT] = eb_dotag(&new->node.branches, EB_LEAF);
        new->node.leaf_p = eb_dotag(root, EB_LEFT);
        new->node.node_p = NULL; /* node part unused */
        return new;
    }

    /* The tree descent is fairly easy :
     *  - first, check if we have reached a leaf node
     *  - second, check if we have gone too far
     *  - third, reiterate
     * Everywhere, we use <new> for the node node we are inserting, <root>
     * for the node we attach it to, and <old> for the node we are
     * displacing below <new>. <troot> will always point to the future node
     * (tagged with its type). <side> carries the side the node <new> is
     * attached to below its parent, which is also where previous node
     * was attached. <newkey> carries the key being inserted.
     */
    newkey = new->key;

    while (1) {
        if (eb_gettag(troot) == EB_LEAF) {
            /* insert above a leaf */
            old = container_of(eb_untag(troot, EB_LEAF), struct eb32_node,
                               node.branches);
            new->node.node_p = old->node.leaf_p;
            up_ptr = &old->node.leaf_p;
            break;
        }

        /* OK we're walking down this link */
        old = container_of(eb_untag(troot, EB_NODE), struct eb32_node,
                           node.branches);
        old_node_bit = old->node.bit;

        /* Stop going down when we don't have common bits anymore. We
         * also stop in front of a duplicates tree because it means we
         * have to insert above.
         */

        if ((old_node_bit < 0) || /* we're above a duplicate tree, stop here */
            (((new->key ^ old->key) >> old_node_bit) >= EB_NODE_BRANCHES)) {
            /* The tree did not contain the key, so we insert <new> before the
             * node <old>, and set ->bit to designate the lowest bit position in
             * <new> which applies to ->branches.b[].
             */
            new->node.node_p = old->node.node_p;
            up_ptr = &old->node.node_p;
            break;
        }

        /* walk down */
        root = &old->node.branches;
        side = (newkey >> old_node_bit) & EB_NODE_BRANCH_MASK;
        troot = root->b[side];
    }

    new_left = eb_dotag(&new->node.branches, EB_LEFT);
    new_rght = eb_dotag(&new->node.branches, EB_RGHT);
    new_leaf = eb_dotag(&new->node.branches, EB_LEAF);

    /* We need the common higher bits between new->key and old->key.
     * What differences are there between new->key and the node here ?
     * NOTE that bit(new) is always < bit(root) because highest
     * bit of new->key and old->key are identical here (otherwise they
     * would sit on different branches).
     */

    // note that if EB_NODE_BITS > 1, we should check that it's still >= 0
    new->node.bit = flsnz(new->key ^ old->key) - EB_NODE_BITS;

    if (new->key == old->key) {
        new->node.bit = -1; /* mark as new dup tree, just in case */

        if (likely(eb_gettag(root_right))) {
            /* we refuse to duplicate this key if the tree is
             * tagged as containing only unique keys.
             */
            return old;
        }

        if (eb_gettag(troot) != EB_LEAF) {
            /* there was already a dup tree below */
            struct eb_node *ret;
            ret = eb_insert_dup(&old->node, &new->node);
            return container_of(ret, struct eb32_node, node);
        }
        /* otherwise fall through */
    }

    if (new->key >= old->key) {
        new->node.branches.b[EB_LEFT] = troot;
        new->node.branches.b[EB_RGHT] = new_leaf;
        new->node.leaf_p = new_rght;
        *up_ptr = new_left;
    } else {
        new->node.branches.b[EB_LEFT] = new_leaf;
        new->node.branches.b[EB_RGHT] = troot;
        new->node.leaf_p = new_left;
        *up_ptr = new_rght;
    }

    /* Ok, now we are inserting <new> between <root> and <old>. <old>'s
     * parent is already set to <new>, and the <root>'s branch is still in
     * <side>. Update the root's leaf till we have it. Note that we can also
     * find the side by checking the side of new->node.node_p.
     */

    root->b[side] = eb_dotag(&new->node.branches, EB_NODE);
    return new;
}

/* Insert eb32_node <new> into subtree starting at node root <root>, using
 * signed keys. Only new->key needs be set with the key. The eb32_node
 * is returned. If root->b[EB_RGHT]==1, the tree may only contain unique keys.
 */
static forceinline struct eb32_node *__eb32i_insert(struct eb_root *root,
                                                    struct eb32_node *new)
{
    struct eb32_node *old;
    unsigned int side;
    eb_troot_t *troot, **up_ptr;
    int newkey; /* caching the key saves approximately one cycle */
    eb_troot_t *root_right;
    eb_troot_t *new_left, *new_rght;
    eb_troot_t *new_leaf;
    int old_node_bit;

    side = EB_LEFT;
    troot = root->b[EB_LEFT];
    root_right = root->b[EB_RGHT];
    if (unlikely(troot == NULL)) {
        /* Tree is empty, insert the leaf part below the left branch */
        root->b[EB_LEFT] = eb_dotag(&new->node.branches, EB_LEAF);
        new->node.leaf_p = eb_dotag(root, EB_LEFT);
        new->node.node_p = NULL; /* node part unused */
        return new;
    }

    /* The tree descent is fairly easy :
     *  - first, check if we have reached a leaf node
     *  - second, check if we have gone too far
     *  - third, reiterate
     * Everywhere, we use <new> for the node node we are inserting, <root>
     * for the node we attach it to, and <old> for the node we are
     * displacing below <new>. <troot> will always point to the future node
     * (tagged with its type). <side> carries the side the node <new> is
     * attached to below its parent, which is also where previous node
     * was attached. <newkey> carries a high bit shift of the key being
     * inserted in order to have negative keys stored before positive
     * ones.
     */
    newkey = new->key + 0x80000000;

    while (1) {
        if (eb_gettag(troot) == EB_LEAF) {
            old = container_of(eb_untag(troot, EB_LEAF), struct eb32_node,
                               node.branches);
            new->node.node_p = old->node.leaf_p;
            up_ptr = &old->node.leaf_p;
            break;
        }

        /* OK we're walking down this link */
        old = container_of(eb_untag(troot, EB_NODE), struct eb32_node,
                           node.branches);
        old_node_bit = old->node.bit;

        /* Stop going down when we don't have common bits anymore. We
         * also stop in front of a duplicates tree because it means we
         * have to insert above.
         */

        if ((old_node_bit < 0) || /* we're above a duplicate tree, stop here */
            (((new->key ^ old->key) >> old_node_bit) >= EB_NODE_BRANCHES)) {
            /* The tree did not contain the key, so we insert <new> before the
             * node <old>, and set ->bit to designate the lowest bit position in
             * <new> which applies to ->branches.b[].
             */
            new->node.node_p = old->node.node_p;
            up_ptr = &old->node.node_p;
            break;
        }

        /* walk down */
        root = &old->node.branches;
        side = (newkey >> old_node_bit) & EB_NODE_BRANCH_MASK;
        troot = root->b[side];
    }

    new_left = eb_dotag(&new->node.branches, EB_LEFT);
    new_rght = eb_dotag(&new->node.branches, EB_RGHT);
    new_leaf = eb_dotag(&new->node.branches, EB_LEAF);

    /* We need the common higher bits between new->key and old->key.
     * What differences are there between new->key and the node here ?
     * NOTE that bit(new) is always < bit(root) because highest
     * bit of new->key and old->key are identical here (otherwise they
     * would sit on different branches).
     */

    // note that if EB_NODE_BITS > 1, we should check that it's still >= 0
    new->node.bit = flsnz(new->key ^ old->key) - EB_NODE_BITS;

    if (new->key == old->key) {
        new->node.bit = -1; /* mark as new dup tree, just in case */

        if (likely(eb_gettag(root_right))) {
            /* we refuse to duplicate this key if the tree is
             * tagged as containing only unique keys.
             */
            return old;
        }

        if (eb_gettag(troot) != EB_LEAF) {
            /* there was already a dup tree below */
            struct eb_node *ret;
            ret = eb_insert_dup(&old->node, &new->node);
            return container_of(ret, struct eb32_node, node);
        }
        /* otherwise fall through */
    }

    if ((s32) new->key >= (s32) old->key) {
        new->node.branches.b[EB_LEFT] = troot;
        new->node.branches.b[EB_RGHT] = new_leaf;
        new->node.leaf_p = new_rght;
        *up_ptr = new_left;
    } else {
        new->node.branches.b[EB_LEFT] = new_leaf;
        new->node.branches.b[EB_RGHT] = troot;
        new->node.leaf_p = new_left;
        *up_ptr = new_rght;
    }

    /* Ok, now we are inserting <new> between <root> and <old>. <old>'s
     * parent is already set to <new>, and the <root>'s branch is still in
     * <side>. Update the root's leaf till we have it. Note that we can also
     * find the side by checking the side of new->node.node_p.
     */

    root->b[side] = eb_dotag(&new->node.branches, EB_NODE);
    return new;
}

#include <stdio.h>
#include <stdlib.h>

#include "timeout.h"

#include "bench.h"

void eb_delete(struct eb_node *node)
{
    __eb_delete(node);
}

struct eb32_node *eb32_insert(struct eb_root *root, struct eb32_node *new)
{
    return __eb32_insert(root, new);
}

struct eb_node *eb_insert_dup(struct eb_node *sub, struct eb_node *new)
{
    return __eb_insert_dup(sub, new);
}

struct ebtimeout {
    struct eb32_node
        node; /* ebtree node used to hold the timer in the timer tree */
    timeout_t expires;
};

struct ebtimeouts {
    timeout_t curtime;
    struct eb_root root;
};

static int timeoutcmp(struct ebtimeout *a, struct ebtimeout *b)
{
    if (a->expires < b->expires) {
        return -1;
    } else if (a->expires > b->expires) {
        return 1;
    } else if (a < b) {
        return -1;
    } else if (a > b) {
        return 1;
    }
    return 0;
}

static void *init(struct timeout *timeout, size_t count, int verbose)
{
    struct ebtimeouts *T;
    size_t i;

    T = malloc(sizeof *T);
    T->curtime = 0;
    T->root = EB_ROOT;

    for (i = 0; i < count; i++) {
        struct ebtimeout *to = (void *) &timeout[i];
        to->expires = 0;
    }

    return T;
}

static void add(void *ctx, struct timeout *_to, timeout_t expires)
{
    struct ebtimeouts *T = ctx;
    struct ebtimeout *to = (void *) _to;

    to->expires = T->curtime + expires;
    to->node.key = to->expires;
    eb32_insert(&T->root, &to->node);
}

static void del(void *ctx, struct timeout *_to)
{
    struct ebtimeout *to = (void *) _to;

    eb32_delete(&to->node);
    to->expires = 0;
}

/*
 * Find the last occurrence of the highest key in the tree <root>, which is
 * equal to or less than <x>. NULL is returned is no key matches.
 */
struct eb32_node *eb32_lookup_le(struct eb_root *root, u32 x)
{
    struct eb32_node *node;
    eb_troot_t *troot;

    troot = root->b[EB_LEFT];
    if (unlikely(troot == NULL))
        return NULL;

    while (1) {
        if ((eb_gettag(troot) == EB_LEAF)) {
            /* We reached a leaf, which means that the whole upper
             * parts were common. We will return either the current
             * node or its next one if the former is too small.
             */
            node = container_of(eb_untag(troot, EB_LEAF), struct eb32_node,
                                node.branches);
            if (node->key <= x)
                return node;
            /* return prev */
            troot = node->node.leaf_p;
            break;
        }
        node = container_of(eb_untag(troot, EB_NODE), struct eb32_node,
                            node.branches);

        if (node->node.bit < 0) {
            /* We're at the top of a dup tree. Either we got a
             * matching value and we return the rightmost node, or
             * we don't and we skip the whole subtree to return the
             * prev node before the subtree. Note that since we're
             * at the top of the dup tree, we can simply return the
             * prev node without first trying to escape from the
             * tree.
             */
            if (node->key <= x) {
                troot = node->node.branches.b[EB_RGHT];
                while (eb_gettag(troot) != EB_LEAF)
                    troot = (eb_untag(troot, EB_NODE))->b[EB_RGHT];
                return container_of(eb_untag(troot, EB_LEAF), struct eb32_node,
                                    node.branches);
            }
            /* return prev */
            troot = node->node.node_p;
            break;
        }

        if (((x ^ node->key) >> node->node.bit) >= EB_NODE_BRANCHES) {
            /* No more common bits at all. Either this node is too
             * small and we need to get its highest value, or it is
             * too large, and we need to get the prev value.
             */
            if ((node->key >> node->node.bit) < (x >> node->node.bit)) {
                troot = node->node.branches.b[EB_RGHT];
                return eb32_entry(eb_walk_down(troot, EB_RGHT),
                                  struct eb32_node, node);
            }

            /* Further values will be too high here, so return the prev
             * unique node (if it exists).
             */
            troot = node->node.node_p;
            break;
        }
        troot =
            node->node.branches.b[(x >> node->node.bit) & EB_NODE_BRANCH_MASK];
    }

    /* If we get here, it means we want to report previous node before the
     * current one which is not above. <troot> is already initialised to
     * the parent's branches.
     */
    while (eb_gettag(troot) == EB_LEFT) {
        /* Walking up from left branch. We must ensure that we never
         * walk beyond root.
         */
        if (unlikely(eb_clrtag((eb_untag(troot, EB_LEFT))->b[EB_RGHT]) == NULL))
            return NULL;
        troot = (eb_root_to_node(eb_untag(troot, EB_LEFT)))->node_p;
    }
    /* Note that <troot> cannot be NULL at this stage */
    troot = (eb_untag(troot, EB_RGHT))->b[EB_LEFT];
    node = eb32_entry(eb_walk_down(troot, EB_RGHT), struct eb32_node, node);
    return node;
}

static struct timeout *get(void *ctx)
{
    struct ebtimeouts *T = ctx;
    struct ebtimeout *to;

    struct eb32_node *n = eb32_lookup_le(&T->root, T->curtime);

    if (n) {
        to = container_of(n, struct ebtimeout, node);
        // fprintf(stdout, "current time: %lu should be greater than expires:
        // %lu\n", T->curtime, to->expires);
        to->expires = 0;
        eb32_delete(&to->node);

        return (void *) to;
    }
    return NULL;
}

static void update(void *ctx, timeout_t ts)
{
    struct ebtimeouts *T = ctx;
    T->curtime = ts;
}

static void check(void *ctx)
{
    return;
}

static int empty(void *ctx)
{
    struct ebtimeouts *T = ctx;
    return eb_is_empty(&T->root);
}

static void destroy(void *ctx)
{
    free(ctx);
}

const struct benchops benchops = {
    .init = &init,
    .add = &add,
    .del = &del,
    .get = &get,
    .update = &update,
    .check = &check,
    .empty = &empty,
    .destroy = &destroy,
};
