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

    public module Egg {
        public type Value = {
            #Unique      : {indexType : IndexStoreType; data : IndexData};
            #SingleField : {indexType : IndexStoreType; data : IndexData};
        };

        public type IndexStoreType = {
            #HashIndex    : IndexData; 
            #OrderedIndex : IndexData;
        };

        public type IndexData = {
            name        : Text;
            target      : Text;
        };

        public func indexFromEgg(e : Egg.Value) : Types.Value {
            switch(e) {
                case (#Unique(d)) {
                    return {
                        name = d.data.name;
                        target = d.data.target;
                        var value = #Unique(getStoreType(d.indexType));
                    }
                };
                case (#SingleField(d)) {
                    return {
                        name = d.data.name;
                        target = d.data.target;
                        var value = #SingleField(getStoreType(d.indexType));
                    };
                };
            };
        };

        func getStoreType(v : IndexStoreType) : IndexType {
            switch(v) {
                case (#HashIndex(_)) {
                    return #Hash(getEmptyHashStore());
                };
                case (#OrderedIndex(_)) {
                    return #Ordered(getEmptyOrderedStore());
                };
            }
        };

        func getEmptyHashStore() : HashIndex.Type {
            return Trie.empty();
        };

        func getEmptyOrderedStore() : OrderedIndex.Type {
            return RBTree.empty();
        };
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