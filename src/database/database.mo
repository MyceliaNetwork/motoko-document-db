import Blob "mo:base/Blob";
import Collection "mo:base/Array";
import Document "mo:base/Array";
import ExperimentalStorage "mo:base/ExperimentalStableMemory";
import Hash "mo:base/Hash";
import Int "mo:base/Int";
import List "mo:base/List";
import Nat "mo:base/Nat";
import Principal "mo:base/Principal";
import Result "mo:base/Result";
import Text "mo:base/Text";
import Trie "mo:base/Trie";
import RBTree "RBTree";

module {
    module Value = {
            public type Value = {
                #Nat       : Nat            ;
                #Int       : Int            ;
                #Text      : Text           ;
                #Blob      : Blob           ;
                #Principal : Principal      ;
                #Document  : Document.Value ; // HZL TODO : Do we really want to support nested documents? Or, should this be a refence type. 
                #Optional  : ?Value         ; 
            };
            public type Type = {
                #Nat                       ;
                #Int                       ;
                #Text                      ;
                #Blob                      ;
                #Principal                 ;
                #Document   : Document.Type;
                #Optional   : ?Type        ;
            };

            public func hash(v : Value.Value) : Hash.Hash {
                switch(v) {
                    case (#Nat(v)) {Hash.hash v};
                    case (#Int(v)) {Int.hash v};
                    case (#Text(v)) {Text.hash v};
                    case (#Principal(v)) {Principal.hash v};
                    case (#Blob(v)) {Blob.hash v};
                    case (#Document(v)) {Document.hash(v)};
                    case (#Optional(v)) {
                        switch(v) {
                            case null return 0;
                            case (?value) return hash(value);
                        };
                    };
                };
            };

            public func key(v : Value) : Trie.Key<Value> {
                {key = v; hash = hash(v)};
            };

            public func typeOf(v : Value) : Type {
                switch(v) {
                    case (#Nat(_))       {#Nat};
                    case (#Int(_))       {#Int};
                    case (#Text(_))      {#Text};
                    case (#Principal(_)) {#Principal};
                    case (#Document(v))  {#Document(v.docType)};
                    case (#Blob(_))      {#Blob};
                    case (#Optional(v))  {
                        switch(v) {
                            case null     return #Optional(null);
                            case (?value) return #Optional(?typeOf(value));
                        };
                    };
                };
            };

            public func typeHash(v : Type) : Hash.Hash {
                switch (v) {
                    case (#Nat(_))       {0};
                    case (#Int(_))       {1};
                    case (#Text(_))      {2};
                    case (#Principal(_)) {3};
                    case (#Document(v))  {4};
                    case (#Blob(_))      {5};
                    case (#Optional(v))  {
                        switch(v) {
                            case (null)   return 6;
                            case (?value) return typeHash(value);
                        };
                    };
                };
            };

            public func sameType(l : Value.Type, r : Value.Type) : Bool {
                return typeHash(l) == typeHash(r);
            };
    };
    
    module Document {
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

    module CollectionIndex = {
        public type Type = {
            #Unique     ;
            #SingleField;
            //#Compound   ;
        };

        type Document  = Document.Value           ;
        type Documents = List.List<Document.Value>;

        module HashIndex = {
            public type Type<V> = Trie.Trie<Value.Value, V>; 

            public func addToIndex() {

            };
        };

        public type IndexType<V> = {
            #Hash    : HashIndex.Type<V>          ;  // Non ordered index stashed in a Binary Hash Trie
            #Ordered : RBTree.Tree<Value.Value, V>;  // Ordered index stashed in a RBTree
        };
        
        module UniqueIndex = {
            public type Type = IndexType<Document.Value>;  // One to One Index
            public func addToIndex(idx : Type, k : Value.Value, v : Document.Value) {

            };
        };

        public type SingleField = IndexType<Documents>;  // One to Many Index

        //public type CompoundIndexValue = Text;
        //public type Compound    = {data : Trie.Trie<Value.Value, List.List<Document.Value>>; qualifier : [Text]};
        public type Data = {
            #Unique      : UniqueIndex.Type;
            #SingleField : SingleField;
            //#Compound    : Compound   ; Lets figure this out later..
        };

        public type Value = {
            name      : Text;       // Reference name of the index
            //typeName  : Type;       // Type of the index. Ie : Unique
            var value : Data;       // Backing Datastore of the index
            target    : Text;       // Collection Field the index is targeting
        };

        public func putDocument(idx : Value, d : Document.Value) : () {
            switch(Document.getFieldValue(d, idx.target)) {
                case null {return};
                case (?value) {

                };
            };
        };

        func putValue(idx : Value, k : Value.Value, d : Document.Value) : Result.Result<(), ()>{
            switch(idx.value) {
                //case (#Compound(compound)) {};
                case (#Unique(idxData)) {};
                case (#SingleField(idxData)) {}
            };
            #ok(); // HZL TODO
        };

        func handleSingleField(idx : Value, idxData : SingleField, k : Value.Value, d : Document.Value) : Result.Result<(), ()> {
            switch(idxData) {
                case (#Hash(data)) {};
                case (#Ordered(data)) {};
            };
            #ok(); // HZL TODO
        };
    };

    module Collection {
        public type Type  = {#Collection};
        public type Value = {
            var autoId        : Nat                             ;  // Next id for a document on create
            typeName          : Text                            ;  // Name of the collection. Ie "Cars"
            var structure     : Trie.Trie<Text, Value.Type>     ;  // Structure of collection Record Field Name -> ValueType
            var documents     : Trie.Trie<Nat, Document.Value>  ;  // Backing Data
            var indicies      : List.List<CollectionIndex.Value>;  // Search Indices 
        };

        public type InsertionResult = Result.Result<Nat, {#StructureError}>;

        public func create(c : Collection.Value, v : [(Text, Value.Value)]) : InsertionResult {
            switch(buildDocumentForCollection(c, v)) {
                case (#err(v)) {
                    #err(v);
                };
                case (#ok(v)) {
                    c.documents := Trie.put(c.documents, {key = v.id; hash = Hash.hash(v.id)}, Nat.equal, v).0;
                    #ok(v.id);
                };
            }
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
        public type Type = {#Database};
        public type Value = {
            name : Text;
            collections : Trie.Trie<Text, Collection.Value>;
        };
    };

    func keyFromText(v : Text) : Trie.Key<Text> {
            return {key = v; hash = Text.hash(v)};
    };
}