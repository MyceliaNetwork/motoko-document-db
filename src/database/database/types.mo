import Trie "mo:base/Trie";
import Collection "../collection/types";

module Types {
    public type Type  = {#Database};
    public type Value = {
        name        : Text;
        collections : Trie.Trie<Text, Collection.Value>;
    };
}