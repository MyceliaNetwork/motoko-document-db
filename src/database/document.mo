import Blob "mo:base/Blob";
import Collection "mo:base/Array";
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
import RBTree "RBTree";
import Result "mo:base/Result";
import Resust "mo:base/Result";
import Text "mo:base/Text";
import Trie "mo:base/Trie";

import Value "value";
import Shared "shared";

import H "helpers";

module Document {
    public type Type       = Shared.Collection.Value;
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
        return Trie.find(v.docType.structure, H.keyFromText(f), Text.equal);
    };

    public func getFieldValue(v : Document.Value, f : Text) : ?Value.Value {
        return Trie.find(v.data, H.keyFromText(f), Text.equal);
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
        d.data := Trie.put<Text, Value.Value>(d.data, H.keyFromText(f), Text.equal, v).0;
        
        #ok();
    };

    public type ValueOfFieldResult = Result.Result<Value.Value, {#FieldNotFound}>;
    public func valueOfField(d : Document.Value, f : Text) : ValueOfFieldResult {
        switch(Trie.find(d.data, H.keyFromText(f), Text.equal)) {
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