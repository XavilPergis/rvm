pub mod def;

pub use self::def::{
    instruction_name, parse_instructions_into, Instruction, InstructionEntry, INSTRUCTION_NAMES,
};

use petgraph::graph::DiGraph;
use std::collections::{btree_map::BTreeMap, BTreeSet, HashMap};

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct Range {
    pub start: usize,
    pub end: usize,
}

impl Range {
    pub fn inverted(self) -> Self {
        Range {
            start: self.end,
            end: self.start,
        }
    }

    pub fn overlaps(&self, other: &Range) -> bool {
        self.contains(other.start)
            || self.contains(other.end)
            || other.contains(self.start)
            || other.contains(self.end)
    }

    pub fn contains(&self, point: usize) -> bool {
        self.start <= point && self.end >= point || self.end <= point && self.start >= point
    }
}

impl From<std::ops::Range<usize>> for Range {
    fn from(range: std::ops::Range<usize>) -> Self {
        Range {
            start: range.start,
            end: range.end,
        }
    }
}

/// Split jump/branch ranges into layers where none of the ranges in a single
/// layer overlap.
pub fn partiton_jumps(intervals: &[Range]) -> Vec<Vec<Range>> {
    let mut layers: Vec<Vec<Range>> = vec![vec![]];

    for &interval in intervals {
        // start at top, find minimum free layer, insert there.
        let mut min_layer = layers.len();
        for (idx, layer) in (0..layers.len()).zip(layers.iter()).rev() {
            // Merging into a layer is possible if the layer is empty or the last item does
            // not overlap with the current one
            if layer
                .last()
                .map(|&last| !last.overlaps(&interval))
                .unwrap_or(true)
            {
                min_layer = idx;
            }
        }

        // Push the interval to the correct layer, creating it if it does not exist.
        if min_layer == layers.len() {
            layers.push(vec![interval]);
        } else {
            layers[min_layer].push(interval);
        }
    }

    layers
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum BranchType {
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanEqual,
    GreaterThanEqual,
    EqualInt,
    NotEqualInt,
    LessThanInt,
    GreaterThanEqualInt,
    GreaterThanInt,
    LessThanEqualInt,
    EqualRef,
    NotEqualRef,
    Null,
    NonNull,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum Jump<T> {
    Return,

    Unconditional(T),
    Branch(BranchType, T),
}

impl<T> Jump<T> {
    pub fn jump_data(&self) -> Option<&T> {
        match self {
            Jump::Unconditional(n) | Jump::Branch(_, n) => Some(n),

            _ => None,
        }
    }

    pub fn map<U, F>(self, func: F) -> Jump<U>
    where
        F: FnOnce(T) -> U,
    {
        match self {
            Jump::Unconditional(n) => Jump::Unconditional(func(n)),
            Jump::Branch(t, n) => Jump::Branch(t, func(n)),
            Jump::Return => Jump::Return,
        }
    }
}

fn get_jump_type(instruction: &Instruction) -> Option<Jump<isize>> {
    Some(match instruction {
        Instruction::IfGreaterThanEqualInt(off) => {
            Jump::Branch(BranchType::GreaterThanEqualInt, *off as isize)
        }
        Instruction::IfGreaterThanEqual(off) => {
            Jump::Branch(BranchType::GreaterThanEqual, *off as isize)
        }
        Instruction::IfLessThanEqualInt(off) => {
            Jump::Branch(BranchType::LessThanEqualInt, *off as isize)
        }
        Instruction::IfGreaterThanInt(off) => {
            Jump::Branch(BranchType::GreaterThanInt, *off as isize)
        }
        Instruction::IfLessThanEqual(off) => Jump::Branch(BranchType::LessThanEqual, *off as isize),
        Instruction::IfNotEqualRef(off) => Jump::Branch(BranchType::NotEqualRef, *off as isize),
        Instruction::IfGreaterThan(off) => Jump::Branch(BranchType::GreaterThan, *off as isize),
        Instruction::IfNotEqualInt(off) => Jump::Branch(BranchType::NotEqualInt, *off as isize),
        Instruction::IfLessThanInt(off) => Jump::Branch(BranchType::LessThanInt, *off as isize),
        Instruction::IfLessThan(off) => Jump::Branch(BranchType::LessThan, *off as isize),
        Instruction::IfNotEqual(off) => Jump::Branch(BranchType::NotEqual, *off as isize),
        Instruction::IfEqualRef(off) => Jump::Branch(BranchType::EqualRef, *off as isize),
        Instruction::IfEqualInt(off) => Jump::Branch(BranchType::EqualInt, *off as isize),
        Instruction::IfNonNull(off) => Jump::Branch(BranchType::NonNull, *off as isize),
        Instruction::IfNull(off) => Jump::Branch(BranchType::Null, *off as isize),
        Instruction::IfEqual(off) => Jump::Branch(BranchType::Equal, *off as isize),

        Instruction::Goto(off) => Jump::Unconditional(*off as isize),

        Instruction::Ret(_)
        | Instruction::ReturnDouble
        | Instruction::ReturnFloat
        | Instruction::ReturnInt
        | Instruction::ReturnLong
        | Instruction::ReturnRef
        | Instruction::ReturnVoid => Jump::Return,

        _ => return None,
    })
}

// TODO: reduce number of allocations; reuse buffers
pub fn create_control_flow_graph(
    instructions: &[InstructionEntry],
) -> DiGraph<(Option<Jump<usize>>, Range), Option<bool>> {
    // If there are no instructions, then there isn't a graph to generate! This also
    // satisfies an invariant that there is at least one block header per
    // instruction listing.
    if instructions.len() == 0 {
        return DiGraph::default();
    }

    // map of byte pos -> index
    let mut byte_map = BTreeMap::new();
    for (instruction, idx) in instructions.iter().zip(0..) {
        byte_map.insert(instruction.start, idx);
    }

    // Set of instructions that are the start points for basic blocks. Either the
    // target of a jump/branch, or the instruction after one.
    let mut headers = BTreeSet::new();

    // The first instruction is a header
    headers.insert(0);

    {
        let mut insert = |start, offset| {
            let end = (start as isize + offset) as usize;
            // header after the jump instruction
            headers.insert(byte_map[&start] + 1);
            // header at the jump target
            headers.insert(byte_map[&end]);
        };

        for InstructionEntry {
            start, instruction, ..
        } in instructions
        {
            match get_jump_type(instruction)
                .and_then(|instruction| instruction.jump_data().cloned())
            {
                Some(off) => insert(*start as usize, off),
                None => (),
            }
        }
    }

    let mut graph = DiGraph::new();

    // Map of header positions -> node indices. We need this to be able to assemble
    // links between jump targets.
    let mut header_map = HashMap::new();

    // Construct a list of all the basic blocks, along with an outgoing jump
    // statement if there was one in the block. This list is sorted in the order
    // that the blocks appear in code. We need this to be able to attach the "next"
    // block in a conditional jump.
    let mut blocks = Vec::new();

    // A sliding window iterator. It has `instructions.len()` tacked onto the end
    // because when you have an iterator that produces `N` values, a sliding window
    // iterator will give you `N-1` items. Since we want to turn `N` header
    // locations into `N` basic blocks, we need this. Note that the value is
    // `instructions.len()` because we subtract one from the end index when
    // constructing the block range, and we want the last range to be the one from
    // `[prev, last instruction]`.
    let iter = headers.iter().cloned().zip(
        headers
            .iter()
            .cloned()
            .skip(1)
            .chain(std::iter::once(instructions.len())),
    );

    // Populate the block list and "header map" (described above) with references to
    // the nodes we insert into the block graph.
    for (current, next) in iter {
        // The range covered by this basic block
        let range = Range {
            start: current,
            end: next - 1,
        };

        // Get the instruction at the end of this block and insert its jump destination
        // if it is a jump
        let instruction = &instructions[next - 1];
        let jump_target = get_jump_type(&instruction.instruction).map(|jump| {
            jump.map(|offset| {
                let end = (instruction.start as isize + offset) as usize;
                byte_map[&end]
            })
        });

        // Populate lists
        let node = graph.add_node((jump_target, range));
        header_map.insert(current, node);
        blocks.push(node);
    }

    // Look at each basic block and connect its outgoing edges
    for i in 0..blocks.len() {
        let (target, _) = graph[blocks[i]];

        match target {
            // Basic block with no jumps; control just flows directly through to the next
            None => {
                if let Some(&next) = blocks.get(i + 1) {
                    graph.add_edge(blocks[i], next, None);
                }
            }

            // Only connect to target; control flow cannot fall through to the next block
            Some(Jump::Unconditional(target)) => {
                let target_node = header_map[&target];
                graph.add_edge(blocks[i], target_node, None);
            }

            // Conditional jump; sometimes will fall through to the next block, sometimes won't, so
            // we add both edges.
            Some(Jump::Branch(_, target)) => {
                let target_node = header_map[&target];
                graph.add_edge(blocks[i], target_node, Some(true));
                if let Some(&next) = blocks.get(i + 1) {
                    graph.add_edge(blocks[i], next, Some(false));
                }
            }

            // Returns don't have any outgoing edges within a subroutine.
            Some(Jump::Return) => (),
        }
    }

    graph
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum InstructionCategory {
    LoadAndStore,
    OperandStackManagement,
    ArithmeticAndLogic,
    TypeConversion,
    ControlFlow,
    InvocationAndReturn,
    ObjectManipulation,
    Other,
}

pub fn get_category(instruction: &Instruction) -> InstructionCategory {
    use self::Instruction::*;
    match instruction {
        LoadInt(_) | LoadLong(_) | LoadFloat(_) | LoadDouble(_) | LoadRef(_) | StoreInt(_)
        | StoreLong(_) | StoreFloat(_) | StoreDouble(_) | StoreRef(_) | LoadArrayInt
        | LoadArrayLong | LoadArrayFloat | LoadArrayDouble | LoadArrayRef | LoadArrayBool
        | LoadArrayChar | LoadArrayShort | StoreArrayInt | StoreArrayLong | StoreArrayFloat
        | StoreArrayDouble | StoreArrayRef | StoreArrayBool | StoreArrayChar | StoreArrayShort
        | ConstNull | ConstInt(_) | ConstLong(_) | ConstFloat(_) | LoadConstant(_)
        | ConstDouble(_) => InstructionCategory::LoadAndStore,

        PushByte(_) | PushShort(_) | Pop | Pop2 | Dup | DupX1 | DupX2 | Dup2 | Dup2X1 | Dup2X2
        | Swap => InstructionCategory::OperandStackManagement,

        AddInt | AddLong | AddFloat | AddDouble | SubInt | SubLong | SubFloat | SubDouble
        | MulInt | MulLong | MulFloat | MulDouble | DivInt | DivLong | DivFloat | DivDouble
        | RemInt | RemLong | RemFloat | RemDouble | NegInt | NegLong | NegFloat | NegDouble
        | ShlInt | ShlLong | ShrInt | ShrLong | LogicalShrInt | LogicalShrLong | AndInt
        | AndLong | OrInt | OrLong | XorInt | XorLong | IncInt | CompareLong | CompareLessFloat
        | CompareGreaterFloat | CompareLessDouble | CompareGreaterDouble => {
            InstructionCategory::ArithmeticAndLogic
        }

        IntToLong | IntToFloat | IntToDouble | LongToInt | LongToFloat | LongToDouble
        | FloatToInt | FloatToLong | FloatToDouble | DoubleToInt | DoubleToLong | DoubleToFloat
        | IntToBool | IntToChar | IntToShort => InstructionCategory::TypeConversion,

        IfEqual(_)
        | IfNotEqual(_)
        | IfLessThan(_)
        | IfGreaterThan(_)
        | IfLessThanEqual(_)
        | IfGreaterThanEqual(_)
        | IfEqualInt(_)
        | IfNotEqualInt(_)
        | IfLessThanInt(_)
        | IfGreaterThanEqualInt(_)
        | IfGreaterThanInt(_)
        | IfLessThanEqualInt(_)
        | IfEqualRef(_)
        | IfNotEqualRef(_)
        | IfNull(_)
        | IfNonNull(_)
        | Goto(_)
        | Jsr(_)
        | Ret(_)
        | Tableswitch
        | Lookupswitch => InstructionCategory::ControlFlow,

        ReturnInt
        | ReturnLong
        | ReturnFloat
        | ReturnDouble
        | ReturnRef
        | ReturnVoid
        | InvokeVirtual(_)
        | InvokeSpecial(_)
        | InvokeStatic(_)
        | InvokeInterface(_, _)
        | InvokeDynamic(_) => InstructionCategory::InvocationAndReturn,

        GetStatic(_)
        | PutStatic(_)
        | GetField(_)
        | PutField(_)
        | New(_)
        | NewArrayPrimitive(_)
        | NewArrayRef(_)
        | NewArrayMultiRef(_, _) => InstructionCategory::ObjectManipulation,

        _ => InstructionCategory::Other,
    }
}
