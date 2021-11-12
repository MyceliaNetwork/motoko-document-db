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
import RBTree "RBTree";
import Result "mo:base/Result";
import Resust "mo:base/Result";
import Text "mo:base/Text";
import Trie "mo:base/Trie";

import Value "value";
import Document "document";
import Shared "shared";


module {
    public type Type  = {#Database};
        public type Value = {
            name        : Text;
            collections : Trie.Trie<Text, Shared.Collection.Value>;
    };

    func keyFromText(v : Text) : Trie.Key<Text> {
            return {key = v; hash = Text.hash(v)};
    };
}