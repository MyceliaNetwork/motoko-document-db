import Trie "mo:base/Trie";
import Values "../values/types";

module Types {
    public type Value = {
            id            : Nat                          ;
            var data      : Trie.Trie<Text, Values.Value> ;
    };
    public type PublicValue = {
        id            : Nat                          ;
        data          : Trie.Trie<Text, Values.Value> ;
    };
    public type Egg = {
        values   : [(Text, Values.Value)];
        typeName : Text;
    };
    public func toPublic(d : Value) : PublicValue {
        return {
            data    = d.data;
            id      = d.id;
        };
    };
}