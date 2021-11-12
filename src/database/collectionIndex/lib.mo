import Blob "mo:base/Blob";

import Debug "mo:base/Debug";
import ExperimentalStorage "mo:base/ExperimentalStableMemory";
import Float "mo:base/Float";
import Hash "mo:base/Hash";
import Int "mo:base/Int";
import Int64 "mo:base/Int64";
import List "mo:base/List";
import Nat "mo:base/Nat";
import Nat64 "mo:base/Nat64";
import Option "mo:base/Option";
import Optiona "mo:base/Option";
import Order "mo:base/Order";
import P "mo:base/Prelude";
import Principal "mo:base/Principal";
import RBTree "../RBTree";
import Result "mo:base/Result";
import Resust "mo:base/Result";
import Text "mo:base/Text";
import Trie "mo:base/Trie";
import Values "../values";

import DocumentTypes "../document/types";
import CollectionIndexTypes "../collectionIndex/types";

module CollectionIndex = {
    public type Type = CollectionIndex.Type;

    type Document  = DocumentTypes.Value           ;
    type Documents = List.List<DocumentTypes.Value>; // HZL This isn't ideal. Should use a TrieSet.

    public type IndexValue = CollectionIndexTypes.IndexValue;
    
    // Index Value Store Types
    module HashIndex = {
        public type Type = CollectionIndexTypes.HashIndex.Type;

        // Add a key to the index. Overwriting the existing value
        public func addToIndex(idx : Type, k : Value.Value, v : IndexValue) : (Type, ?IndexValue) {
            let key = Value.key(k);
            return Trie.put<Value.Value, IndexValue>(idx, key, Value.equal, v);
        };

        // Find a value in the index
        public func findInIndex(idx : Type, k : Value.Value) : ?IndexValue {
            let key = Value.key(k);
            return Trie.find(idx, key, Value.equal);
        };
    };

    module OrderedIndex = {
        public type Type = CollectionIndexTypes.OrderedIndex.Type;

        // Add a key to the index. Overwriting the existing value
        public func addToIndex(idx : Type, k : Value.Value, v : IndexValue) : (Type, ?IndexValue) {
            let (a, b) = RBTree.put<Value.Value, IndexValue>(idx, Value.compare, k, v);
            return (b, a);
        };
        
        public func findInIndex(idx : Type, k : Value.Value) : ?IndexValue {
            RBTree.get(idx, Value.compare, k);
        };
    };

    // Index Types
    public type IndexType = CollectionIndexTypes.IndexType;
    
    module UniqueIndex = {
        public type Type = IndexType;  // One to One Index
        public type Error = CollectionIndexTypes.UniqueIndex.Error;

        public func addToIndex(idx : Type, k : Value.Value, v : Shared.Document.Value) : Resust.Result<Type, Error> {
            switch(idx) {
                case (#Hash(data)) {
                    switch(HashIndex.findInIndex(data, k)) {
                        case null {
                            return #ok(#Hash(HashIndex.addToIndex(data, k, #Document(v)).0));
                        };
                        case (?_) return #err(#AlreadyExists);
                    };
                };
                case (#Ordered(data)) {
                    switch(OrderedIndex.findInIndex(data, k)) {
                        case null {
                            return #ok(#Ordered(OrderedIndex.addToIndex(data, k, #Document(v)).0));
                        };
                        case (?_) return #err(#AlreadyExists);
                    };
                };
            };
        };
    };

    module SingleField = {
        public type Type = IndexType;  // One to Many Index
        public type Error = CollectionIndexTypes.SingleField.Error;

        public func addToIndex(idx : Type, k : Value.Value, v : Shared.Document.Value) : Resust.Result<Type, Error> {
            switch (idx) {
                case (#Hash(data)) {
                    switch(HashIndex.findInIndex(data, k)) {
                        case null {
                            return #ok(#Hash(HashIndex.addToIndex(data, k, #Documents(List.make(v))).0));
                        };
                        case (?existing) {
                            switch(existing) {
                                case (#Documents(documents)) {
                                    return #ok(#Hash(HashIndex.addToIndex(data, k, #Documents(List.push(v, documents))).0));
                                };
                                case (_) {P.unreachable()};
                            };
                        }
                    };
                };
                case (#Ordered(data)) {
                    switch(OrderedIndex.findInIndex(data, k)) {
                        case null {
                            return #ok(#Ordered(OrderedIndex.addToIndex(data, k, #Documents(List.make(v))).0));
                        };
                        case (?existing) {
                            switch(existing) {
                                case (#Documents(documents)) {
                                    return #ok(#Ordered(OrderedIndex.addToIndex(data, k, #Documents(List.push(v, documents))).0));
                                };
                                case (_) {P.unreachable()};
                            };
                        }
                    };
                };
            };
        };
    };


    //public type CompoundIndexValue = Text;
    //public type Compound    = {data : Trie.Trie<Value.Value, List.List<Document.Value>>; qualifier : [Text]};
    public type Data = {
        #Unique      : UniqueIndex.Type;
        #SingleField : SingleField.Type;
        //#Compound    : Compound   ; Lets figure this out later..
    };

    public type Value = CollectionIndexTypes.Value;
    
    public type Error = {
        #UniqueIndexError : UniqueIndex.Error;
        #SingleFieldError : SingleField.Error;
    };

    public func addToIndex(idx : Value, k : Value.Value, d : Shared.Document.Value) : Result.Result<Data, Error>{
        switch(idx.value) {
            //case (#Compound(compound)) {};
            case (#Unique(idxData)) {
                //Debug.print("Adding to Unique Index");
                switch(UniqueIndex.addToIndex(idxData, k, d)) {
                    case (#ok(new)) {return #ok (#Unique(new))};
                    case (#err(e))  {return #err(#UniqueIndexError(e))}
                };
            };
            case (#SingleField(idxData)) {
                //Debug.print("Adding to SingleField Index");
                switch(SingleField.addToIndex(idxData, k, d)) {
                    case (#ok(new)) {return #ok (#SingleField(new))};
                    case (#err(e))  {return #err(#SingleFieldError(e))}
                };
            }
        };
        P.unreachable();
    };

    func handleSingleField(idx : Value, idxData : SingleField.Type, k : Value.Value, d : Shared.Document.Value) : Result.Result<(), ()> {
        switch(idxData) {
            case (#Hash(data)) {};
            case (#Ordered(data)) {};
        };
        #ok(); // HZL TODO
    };
};