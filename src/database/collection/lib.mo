import H "../helpers";
import Hash "mo:base/Hash";
import List "mo:base/List";
import Nat "mo:base/Nat";
import Option "mo:base/Option";
import Result "mo:base/Result";
import Text "mo:base/Text";
import Trie "mo:base/Trie";

import CollectionIndex "../collectionIndex";

import Document "../document/types";
import Values "../values/";

import Types "types";

module Collection {
    public type Type      = Types.Type;
    public type Value     = Types.Value;
    public type Structure = Types.Structure;
    public type Documents = Types.Documents;
    public type Indices   = Types.Indices;

    public type InsertionError = {
        #StructureError                              ;
        #IndexError    : CollectionIndex.Error       ;
    };


    public func getEmptyCollection(name : Text) : Types.Value {
        return  {
            typeName   = name;
            var autoId = 0;
            var documents : Documents = Trie.empty();
            var indicies  : Indices   = Trie.empty();
            var structure : Structure = Trie.empty();
        }
    };

    public type InsertionResult = Result.Result<Nat, InsertionError>;

    public func create(c : Collection.Value, v : [(Text, Values.Value)]) : InsertionResult {
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
        Trie.find(c.indicies, H.keyFromText(f), Text.equal);
    };

    func indexValidateAndFinalize(c : Collection.Value, d : Document.Value) : InsertionResult {
        var updatedIndicies : Trie.Trie<Text, List.List<CollectionIndex.Value>> = Trie.empty();

        for (field in Trie.iter(d.data)) {
            switch(getIndicesForField(c, field.0)) {
                case null {};
                case (?indicies) {
                    switch(tryAdd(field.1, d, indicies)) {
                        case (#ok(updated)) {
                            updatedIndicies := Trie.put(updatedIndicies, H.keyFromText(field.0), Text.equal, updated).0;
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

    func tryAdd(v : Values.Value, d : Document.Value, idxs : List.List<CollectionIndex.Value>) : Result.Result<List.List<CollectionIndex.Value>, CollectionIndex.Error> {
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

    public func typeOfField(c : Collection.Value, f : Text) : ?Values.Type {
        Trie.find(c.structure, H.keyFromText(f), Text.equal)
    };

    func buildDocumentForCollection(c : Collection.Value, v : [(Text, Values.Value)]) : Result.Result<Document.Value, {#StructureError}> {
        var values : Trie.Trie<Text, Values.Value> = Trie.empty();
        var entryCount    = 0;
        var structureSize = Trie.size(c.structure);
    
        for (value in v.vals()) {
            // Validate document structure
            switch(typeOfField(c, value.0)) {
                case null return #err(#StructureError);
                case (?fieldType) {
                    if (not (Values.sameType(fieldType, Values.typeOf(value.1)))) {
                        return #err(#StructureError);
                    };
                };
            };

            values := Trie.put(values, H.keyFromText(value.0), Text.equal, value.1).0;
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