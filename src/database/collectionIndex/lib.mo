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

import Documents "../document/types";
import Types "types";

module CollectionIndex = {
    public type Value     = Types.Value;
    public type Document  = Documents.Value           ;
    public type Documents = List.List<Documents.Value>; // HZL This isn't ideal. Should use a TrieSet.

    public type IndexValue = Types.IndexValue;
    
    // Index Value Store Types
    module HashIndex = {
        public type Type = Types.HashIndex.Type;

        // Add a key to the index. Overwriting the existing value
        public func addToIndex(idx : Type, k : Values.Value, v : IndexValue) : (Type, ?IndexValue) {
            let key = Values.key(k);
            return Trie.put<Values.Value, IndexValue>(idx, key, Values.equal, v);
        };

        // Find a value in the index
        public func findInIndex(idx : Type, k : Values.Value) : ?IndexValue {
            let key = Values.key(k);
            return Trie.find(idx, key, Values.equal);
        };
    };

    module OrderedIndex = {
        public type Type = Types.OrderedIndex.Type;

        // Add a key to the index. Overwriting the existing value
        public func addToIndex(idx : Type, k : Values.Value, v : IndexValue) : (Type, ?IndexValue) {
            let (a, b) = RBTree.put<Values.Value, IndexValue>(idx, Values.compare, k, v);
            return (b, a);
        };
        
        public func findInIndex(idx : Type, k : Values.Value) : ?IndexValue {
            RBTree.get(idx, Values.compare, k);
        };
    };

    // Index Types
    public type IndexType = Types.IndexType;
    
    module UniqueIndex = {
        public type Type = IndexType;  // One to One Index
        public type Error = Types.UniqueIndex.Error;

        public func addToIndex(idx : Type, k : Values.Value, v : Documents.Value) : Resust.Result<Type, Error> {
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
        public type Error = Types.SingleField.Error;

        public func addToIndex(idx : Type, k : Values.Value, v : Documents.Value) : Resust.Result<Type, Error> {
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
    
    public type Error = {
        #UniqueIndexError : UniqueIndex.Error;
        #SingleFieldError : SingleField.Error;
    };

    public func addToIndex(idx : Value, k : Values.Value, d : Documents.Value) : Result.Result<Data, Error>{
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

    func handleSingleField(idx : Value, idxData : SingleField.Type, k : Values.Value, d : Documents.Value) : Result.Result<(), ()> {
        switch(idxData) {
            case (#Hash(data)) {};
            case (#Ordered(data)) {};
        };
        #ok(); // HZL TODO
    };
};