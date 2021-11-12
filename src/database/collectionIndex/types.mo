import Document "../document/types";
import List "mo:base/List";
import RBTree "../RBTree";
import Trie "mo:base/Trie";
import Values "../values/types";

module Types {
    public type Type = {
        #Unique     ;
        #SingleField;
        //#Compound   ;
    };

    public type Value = {
        name      : Text;       // Reference name of the index
        //typeName  : Type;       // Type of the index. Ie : Unique
        var value : Data;       // Backing Datastore of the index
        target    : Text;       // Collection Field the index is targeting
    };

    public type Data = {
        #Unique      : UniqueIndex.Type;
        #SingleField : SingleField.Type;
        //#Compound    : Compound   ; Lets figure this out later..
    };

    public module UniqueIndex {
        public type Type = IndexType;
        public type Error = {
            #AlreadyExists;
        };
    };

    public module SingleField {
        public type Type = IndexType;
        public type Error = {};
    };

    public type IndexType = {
        #Hash    : HashIndex.Type                      ;  // Non ordered index stashed in a Binary Hash Trie
        #Ordered : RBTree.Tree<Values.Value, IndexValue>;  // Ordered index stashed in a RBTree
    };

    public module HashIndex = {
        public type Type = Trie.Trie<Values.Value, IndexValue>; 
    };

    public module OrderedIndex = {
        public type Type = RBTree.Tree<Values.Value, IndexValue>;
    };

    type Document  = Document.Value           ;
    type Documents = List.List<Document.Value>; // HZL This isn't ideal. Should use a TrieSet.

    public type IndexValue = {
        #Document  : Document;
        #Documents : Documents;
    };

    public type Error = {
        #UniqueIndexError : UniqueIndex.Error;
        #SingleFieldError : SingleField.Error;
    };
}