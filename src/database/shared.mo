import List "mo:base/List";
import Trie "mo:base/Trie";
import RBTree "RBTree";

module Shared {
    public module Collection {
        public type Type  = {#Collection};
        public type Value = {
            var autoId        : Nat                             ;  // Next id for a document on create
            typeName          : Text                            ;  // Name of the collection. Ie "Cars"
            var structure     : Trie.Trie<Text, Value.Type>     ;  // Structure of collection Record Field Name -> ValueType
            var documents     : Trie.Trie<Nat, Document.Value>  ;  // Backing Data
            var indicies      : Trie.Trie<Text, List.List<CollectionIndex.Value>>;  // Search Indices 
        };
    };

    public module CollectionIndex {
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
            #Ordered : RBTree.Tree<Value.Value, IndexValue>;  // Ordered index stashed in a RBTree
        };

        public module HashIndex = {
            public type Type = Trie.Trie<Value.Value, IndexValue>; 
        };

        public module OrderedIndex = {
            public type Type = RBTree.Tree<Value.Value, IndexValue>;
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
    };

    public module Value {
        public type Value = {
            #Nat       : Nat64                 ;
            #Int       : Int64                 ;
            #Float     : Float                 ;
            #Text      : Text                  ;
            #Blob      : Blob                  ;
            #Principal : Principal             ;
            //#Document  : Document.Value        ; // HZL TODO : Do we really want to support nested documents? Or, should this be a refence type. {#Local : Document.Value; #Remote ...}
            #Optional  : {v : ?Value; t : Type}; 
        };

        public type Type = {
            #Nat                               ;
            #Int                               ;
            #Float                             ;
            #Text                              ;
            #Blob                              ;
            #Principal                         ;
            //#Document   : Document.Type        ;
            #Optional   : Type                 ;
        };
    };

    public module Document {
        public type Value = {
            id            : Nat                          ;
            docType       : Collection.Value             ;
            var data      : Trie.Trie<Text, Value.Value> ;
        };
        public type PublicValue = {
            id            : Nat                          ;
            docType       : Text                         ;
            data          : Trie.Trie<Text, Value.Value> ;
        };
        public type Egg         = {
            values   : [(Text, Value.Value)];
            typeName : Text;
        };

        public func toPublic(d : Value) : PublicValue {
            return {
                data    = d.data;
                docType = d.docType.typeName;
                id      = d.id;
            };
        };
    };
};