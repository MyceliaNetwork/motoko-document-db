import Blob "mo:base/Blob";
import Debug "mo:base/Debug";

import ExperimentalStorage "mo:base/ExperimentalStableMemory";
import Float "mo:base/Float";
import H "../helpers";
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
import Collection "../collection/types";
import Types "types";

module Document {
    public type Value       = Types.Value;
    public type PublicValue = Types.PublicValue;
    public type Egg         = Types.Egg;

    public func hash(v : Value) : Hash.Hash {
        return 0; // HZL TODO!!
    };

    public func getFieldValue(v : Document.Value, f : Text) : ?Values.Value {
        return Trie.find(v.data, H.keyFromText(f), Text.equal);
    };

    public type UpdateResult = Result.Result<(), {#TypeMismatch : Values.Type; #FieldNotFound}>;

    public func updateField(d : Document.Value, f : Text, v : Values.Value) : UpdateResult {
        d.data := Trie.put<Text, Values.Value>(d.data, H.keyFromText(f), Text.equal, v).0;
        #ok();
    };

    public type ValueOfFieldResult = Result.Result<Values.Value, {#FieldNotFound}>;
    public func valueOfField(d : Document.Value, f : Text) : ValueOfFieldResult {
        switch(Trie.find(d.data, H.keyFromText(f), Text.equal)) {
            case null return #err(#FieldNotFound);
            case (?v) return #ok(v);
        };
    };

    public func toPublic(d : Value) : PublicValue {
        return {
            data    = d.data;
            id      = d.id;
        };
    };

    //public func insertField(f : Field, v : Value.Type)
};