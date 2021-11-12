import Text "mo:base/Text";
import Trie "mo:base/Trie";

module Helpers {
    public func keyFromText(v : Text) : Trie.Key<Text> {
            return {key = v; hash = Text.hash(v)};
    };
}