# Current components

### Datatypes: 'Types' folder
- File
- Folder
- Hash
- Inverted Table
- Index

### IO operations:
- Hash file: IO.Hashing
- Construct an index from a folder: Demos.Demo3

### General File system IO operations: System.Directory.Extras
- Get all files in a directory recursively
- Get child files
- Gen child folders

### Util.Show:
- mkString

# Datatypes

### Hash:  
  A ByteString of length 512, currently SHA3-512

### File:  
  Contains only it's Hash.

### Folder:  
  - folders: Mappings from names to Folders
  - files:   Mappings from names to Files

### Note:  
  We did not include the names of the current file/folder inside the datatype
  because otherwise we would have to keep 2 things in sync:
  - The name of the current file/folder
  - The name in the map of the parent  
  which is bug-prone.

### InvertedTable:  
  A mapping from Hashes (ByteStrings) to FilePaths on disk,
  where the file with the given hash is stored.  
  Implemented currently using a ByteString Trie.

### Index:  
  - root: The root folder of the Content Indexed File System.
  - invertedTable: An inverted table for the files in the Content Indexed File System.
