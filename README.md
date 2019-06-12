# amontillado  

[Amontillado](https://en.wikipedia.org/wiki/The_Cask_of_Amontillado) is a simple key-value store, backed by filtered log-structured merge trees, implemented in Haskell. Modelled after DynamoDB/Cassandra (C*)/Riak.

## how to run the sample application 
1. in `src/`, `cp` the `ref_sstable.csv` to `sstable.csv` (the database makes changes to the file passed in on the command line)
2. run `runghc Main.hs --data sstable.csv`
3. hit enter to continue at each step
4. in-memory structures are compacted to `newsstable.csv`

## data model and internals
Records in Amontillado reside in two levels: an in-memory tree ("the Carnival", `MemoryTable.hs`) and an on-disk run table ("the Dungeon", `SortedStringTable`). When the number of records in the in-memory tree exceeds a certain threshold, all writes are serialized to disk into the run table on disk.

## basic features
### Haskell design patterns: 
- Monoid: `MemoryTable.hs` is a monoid, with empty trees as the identity element and a tree merge as the associative operation. 
- Foldable: Folding is possible over `MemoryTable` values.

## impure components: 
- `Main.hs` waits on user input in between executing steps of a SQL-like script of reads and writes. 
- `SortedStringTable` reads/writes key-value pairs to/from a disk.

## advanced features:
- the `MemoryTable` class is implemented as a red-black tree.

## future work
- Garbage collect old compaction files.
- Use the `STM` monad to block writes during compaction.
- Use `forkIO` to run multiple `LSMTree` values, each in a separate thread.
- Use a commit log to ensure in-memory writes are not lost.

## design choices
The `Entry` type has a `Timestamp` field, and the `MemoryTable` implements the `Semigroup` class because the original goal was to have multiple threads serving read and write requests; timestamps would be used to implement last-write-wins consistency and the associative merge of trees would be used to serialize all in-memory state to disk.

Is this a good idea? Probably not. A one-to-one translation of internals of the DynamoDB structures does not utilize the strengths of the functional programming paradigm. The in-memory tree and merge tree design is optimized for append-only data, but the immutability of values in Haskell gives us this for free and can likely use a simpler set of data structures.

Additionally, garbage collection is a known performance issue with Cassandra-style systems; the Java implementation of Cassandra maintains most of its state off-heap to minimize the disruption of the JVM's garbage collection. Re-implementations of Cassandra in purely imperative languages, like ScyllaDB in C++, get around this issue.


## resources
- Okasaki, Chris. _Purely Functional Data Structures_.
- Peyton Jones, Simon. "The Santa Claus Problem". https://www.schoolofhaskell.com/school/advanced-haskell/beautiful-concurrency/4-the-santa-claus-problem
- "Fundamentals of System Design — Part 3." https://hackernoon.com/fundamentals-of-system-design-part-3-8da61773a631
- "On Disk IO, Part 3: LSM Trees." https://medium.com/databasss/on-disk-io-part-3-lsm-trees-8b2da218496f

