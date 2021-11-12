import Blob "mo:base/Blob";
import Collection "mo:base/Array";
import Debug "mo:base/Debug";
import Document "mo:base/Array";
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
import RBTree "RBTree";
import Result "mo:base/Result";
import Resust "mo:base/Result";
import Text "mo:base/Text";
import Trie "mo:base/Trie";
import Value "mo:base/Array";

module {
    public module Value = {
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

            public func hash(v : Value.Value) : Hash.Hash {
                switch(v) {
                    case (#Nat(v)) {Hash.hash(Nat64.toNat(v))};
                    case (#Int(v)) {Int.hash(Int64.toInt(v))};
                    case (#Float(v)) {Text.hash(Float.toText(v))};
                    case (#Text(v)) {Text.hash v};
                    case (#Principal(v)) {Principal.hash v};
                    case (#Blob(v)) {Blob.hash v};
                    //case (#Document(v)) {Document.hash(v)};
                    case (#Optional(v)) {
                        switch(v.v) {
                            case null return 0;
                            case (?value) return hash(value);
                        };
                    };
                };
            };

            public func compare(l : Value.Value, r : Value.Value) : Order.Order {
                assert sameType(typeOf(l), typeOf(r));

                switch(l) {
                    case (#Nat(v)) {
                        return Nat64.compare(v, Unwrap.nat(r));
                    };
                    case (#Int(v)) {
                        return Int64.compare(v, Unwrap.int(r));
                    };
                    case (#Float(v)) {
                        return Float.compare(v, Unwrap.float(r));
                    };
                    case (#Text(v)) {
                        return Text.compare(v, Unwrap.text(r));
                    };
                    case (#Principal(v)) {
                        return Principal.compare(v, Unwrap.principal(r));
                    };
                    case (#Blob(v)) {
                        return Blob.compare(v, Unwrap.blob(r));
                    };
                    // case (#Document(v)) {
                    //     return Nat.compare(v.id, Unwrap.document(r).id);
                    // };
                    case (#Optional(v)) {
                        let rR = Unwrap.optional(r);
                        switch(v.v) {
                            case (null) {
                                if (Option.isNull(rR)) {
                                    return #equal;
                                };
                                return #less;
                            };
                            case (?v)  {
                                switch rR {
                                    case null return #greater;
                                    case (?rRValue) {return compare(v, rRValue)};
                                };
                            };
                        };
                    };
                };
            };
            
            module Unwrap {
                public func nat(v0 : Value) : Nat64 {
                    switch(v0) { case (#Nat(v)) return v; case _ {assert false}};
                    P.unreachable();
                };

                public func int(v0 : Value) : Int64 {
                    switch(v0) { case (#Int(v)) return v; case _ {assert false}};
                    P.unreachable();
                };

                public func float(v0 : Value) : Float {
                    switch(v0) { case (#Float(v)) return v; case _ {assert false}};
                    P.unreachable();
                };

                public func text(v0 : Value) : Text {
                    switch(v0) { case (#Text(v)) return v; case _ {assert false}};
                    P.unreachable();
                };

                public func principal(v0 : Value) : Principal {
                    switch(v0) { case (#Principal(v)) return v; case _ {assert false}};
                    P.unreachable();
                };

                public func blob(v0 : Value) : Blob {
                    switch(v0) { case (#Blob(v)) return v; case _ {assert false}};
                    Blob.fromArray([]);
                };

                // public func document(v0 : Value) : Document.Value {
                //     switch(v0) { case (#Document(v)) return v; case _ {assert false}};
                //     P.unreachable();
                // };

                public func optional(v0 : Value) :?Value.Value {
                    switch(v0) { case (#Optional(v)) return v.v; case _ {assert false}};
                    P.unreachable();
                };
            };

            public func key(v : Value) : Trie.Key<Value> {
                {key = v; hash = hash(v)};
            };

            public func equal(l : Value, r : Value) : Bool {
                return hash(l) == hash(r) and typeHash(typeOf(l)) == typeHash(typeOf(r));
            };

            public func typeOf(v : Value) : Type {
                switch(v) {
                    case (#Nat(_))       {#Nat};
                    case (#Int(_))       {#Int};
                    case (#Text(_))      {#Text};
                    case (#Principal(_)) {#Principal};
                    //case (#Document(v))  {#Document(v.docType)};
                    case (#Blob(_))      {#Blob};
                    case (#Float(_))     {#Float};
                    case (#Optional(v))  {#Optional(v.t)};
                };
            };

            public func typeHash(v : Type) : Hash.Hash {
                switch (v) {
                    case (#Nat(_))       {0};
                    case (#Int(_))       {1};
                    case (#Text(_))      {2};
                    case (#Principal(_)) {3};
                    //case (#Document(v))  {4};
                    case (#Blob(_))      {5};
                    case (#Optional(v))  {return 1000 + typeHash(v) + 1;};
                    case (#Float(_))         {100}
                };
            };

            public func sameType(l : Value.Type, r : Value.Type) : Bool {
                return typeHash(l) == typeHash(r);
            };
    };
    
    public module Document {
        public type Type       = Collection.Value;
        public type Value      = {
            id            : Nat                          ;
            docType       : Type                         ;
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

        public func hash(v : Value) : Hash.Hash {
            return 0; // HZL TODO!!
        };

        public func getFieldType(v : Document.Value, f : Text) : ?Value.Type {
            return Trie.find(v.docType.structure, keyFromText(f), Text.equal);
        };

        public func getFieldValue(v : Document.Value, f : Text) : ?Value.Value {
            return Trie.find(v.data, keyFromText(f), Text.equal);
        };

        public type UpdateResult = Result.Result<(), {#TypeMismatch : Value.Type; #FieldNotFound}>;

        public func updateField(d : Document.Value, f : Text, v : Value.Value) : UpdateResult {
            switch (getFieldType(d, f)) {
                case null return #err(#FieldNotFound);
                case (?fieldType) {
                    if (not Value.sameType(fieldType, Value.typeOf(v))) {
                        return #err(#TypeMismatch(fieldType));
                    };
                };
            };
            d.data := Trie.put<Text, Value.Value>(d.data, keyFromText(f), Text.equal, v).0;
            
            #ok();
        };

        public type ValueOfFieldResult = Result.Result<Value.Value, {#FieldNotFound}>;
        public func valueOfField(d : Document.Value, f : Text) : ValueOfFieldResult {
            switch(Trie.find(d.data, keyFromText(f), Text.equal)) {
                case null return #err(#FieldNotFound);
                case (?v) return #ok(v);
            };
        };

        public func toPublic(d : Value) : PublicValue {
            return {
                data    = d.data;
                docType = d.docType.typeName;
                id      = d.id;
            };
        };

        //public func insertField(f : Field, v : Value.Type)
    };

    public module CollectionIndex = {
        public type Type = {
            #Unique     ;
            #SingleField;
            //#Compound   ;
        };

        type Document  = Document.Value           ;
        type Documents = List.List<Document.Value>; // HZL This isn't ideal. Should use a TrieSet.

        public type IndexValue = {
            #Document  : Document;
            #Documents : Documents;
        };
        
        // Index Value Store Types
        module HashIndex = {
            public type Type = Trie.Trie<Value.Value, IndexValue>; 

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
            public type Type = RBTree.Tree<Value.Value, IndexValue>;

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
        public type IndexType = {
            #Hash    : HashIndex.Type                      ;  // Non ordered index stashed in a Binary Hash Trie
            #Ordered : RBTree.Tree<Value.Value, IndexValue>;  // Ordered index stashed in a RBTree
        };
        
        module UniqueIndex = {
            public type Type = IndexType;  // One to One Index
            public type Error = {
                #AlreadyExists;
            };

            public func addToIndex(idx : Type, k : Value.Value, v : Document.Value) : Resust.Result<Type, Error> {
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
            public type Error = {};
            public func addToIndex(idx : Type, k : Value.Value, v : Document.Value) : Resust.Result<Type, Error> {
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

        public type Value = {
            name      : Text;       // Reference name of the index
            //typeName  : Type;       // Type of the index. Ie : Unique
            var value : Data;       // Backing Datastore of the index
            target    : Text;       // Collection Field the index is targeting
        };
        
        public type Error = {
            #UniqueIndexError : UniqueIndex.Error;
            #SingleFieldError : SingleField.Error;
        };

        public func addToIndex(idx : Value, k : Value.Value, d : Document.Value) : Result.Result<Data, Error>{
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

        func handleSingleField(idx : Value, idxData : SingleField.Type, k : Value.Value, d : Document.Value) : Result.Result<(), ()> {
            switch(idxData) {
                case (#Hash(data)) {};
                case (#Ordered(data)) {};
            };
            #ok(); // HZL TODO
        };
    };

    public module Collection {
        public type Type  = {#Collection};

        public type Structure = Trie.Trie<Text, Value.Type>;
        public type Documents = Trie.Trie<Nat, Document.Value>;
        public type Indices   = Trie.Trie<Text, List.List<CollectionIndex.Value>>;

        public type Value = {
            var autoId        : Nat                             ;  // Next id for a document on create
            typeName          : Text                            ;  // Name of the collection. Ie "Cars"
            var structure     : Trie.Trie<Text, Value.Type>     ;  // Structure of collection Record Field Name -> ValueType
            var documents     : Trie.Trie<Nat, Document.Value>  ;  // Backing Data
            var indicies      : Trie.Trie<Text, List.List<CollectionIndex.Value>>;  // Search Indices 
        };

        public type InsertionError = {
            #StructureError                       ;
            #IndexError    : CollectionIndex.Error;
        };

        public type InsertionResult = Result.Result<Nat, InsertionError>;

        public func create(c : Collection.Value, v : [(Text, Value.Value)]) : InsertionResult {
            switch(buildDocumentForCollection(c, v)) {
                case (#err(v)) {
                    #err(v);
                };
                case (#ok(newDocument)) {
                    return indexValidateAndFinalize(c, newDocument);
                };
            }
        };

        func getIndicesForField(c : Collection.Value, f : Text) : ?List.List<CollectionIndex.Value> {
            Trie.find(c.indicies, keyFromText(f), Text.equal);
        };

        func indexValidateAndFinalize(c : Collection.Value, d : Document.Value) : InsertionResult {
            var updatedIndicies : Trie.Trie<Text, List.List<CollectionIndex.Value>> = Trie.empty();

            for (field in Trie.iter(d.data)) {
                switch(getIndicesForField(c, field.0)) {
                    case null {};
                    case (?indicies) {
                        switch(tryAdd(field.1, d, indicies)) {
                            case (#ok(updated)) {
                                updatedIndicies := Trie.put(updatedIndicies, keyFromText(field.0), Text.equal, updated).0;
                            };
                            case (#err(e)) {
                                return #err(#IndexError(e));
                            };
                        };
                    };
                };
            };

            // Finalize
            c.documents := Trie.put(c.documents, {key = d.id; hash = Hash.hash(d.id)}, Nat.equal, d).0;
            c.indicies  := updatedIndicies;
            c.autoId    += 1;
            #ok(d.id);
        };

        func tryAdd(v : Value.Value, d : Document.Value, idxs : List.List<CollectionIndex.Value>) : Result.Result<List.List<CollectionIndex.Value>, CollectionIndex.Error> {
            var this : List.List<CollectionIndex.Value> = idxs;
            var out  : List.List<CollectionIndex.Value> = List.nil();

            while (Option.isSome(this)) {
                ignore do ? {
                    let idx = this!.0;
                    switch(CollectionIndex.addToIndex(idx, v, d)) {
                        case (#ok(result)) {
                            // HZL TODO - we're generating a silly amount of Garbage with each index.. 
                            // Might consider creating an IndexResultDelta Object 
                            out := List.push<CollectionIndex.Value>({
                                name      = idx.name;
                                target    = idx.target;
                                var value = result;
                            }, out);
                        };
                        case (#err(e)) {
                            return #err(e);
                        };
                    };
                };

                this := List.pop<CollectionIndex.Value>(this).1;
            };
            return #ok(out);
        };

        public func getById(c : Collection.Value, id : Nat) : ?Document.PublicValue {
            switch(Trie.find(c.documents, {key = id; hash = Hash.hash(id)}, Nat.equal)) {
                case null return null;
                case (?v) return ?Document.toPublic(v);
            };
        };

        public func typeOfField(c : Collection.Value, f : Text) : ?Value.Type {
            Trie.find(c.structure, keyFromText(f), Text.equal)
        };

        func buildDocumentForCollection(c : Collection.Value, v : [(Text, Value.Value)]) : Result.Result<Document.Value, {#StructureError}> {
            var values : Trie.Trie<Text, Value.Value> = Trie.empty();
            var entryCount    = 0;
            var structureSize = Trie.size(c.structure);

            for (value in v.vals()) {
                values := Trie.put(values, keyFromText(value.0), Text.equal, value.1).0;
                entryCount += 1;
                if (entryCount > structureSize) {
                    return #err(#StructureError);
                };
            };

            if (entryCount != structureSize) {
                return #err(#StructureError);
            };

            return #ok({
                id      = c.autoId;
                docType  = c      ;
                var data = values ;
            });
        };
    };

    module Database {
        public type Type  = {#Database};
        public type Value = {
            name        : Text;
            collections : Trie.Trie<Text, Collection.Value>;
        };
    };

    func keyFromText(v : Text) : Trie.Key<Text> {
            return {key = v; hash = Text.hash(v)};
    };
}