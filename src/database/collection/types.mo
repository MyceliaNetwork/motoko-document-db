import Trie "mo:base/Trie";
import List "mo:base/List";

import Values "../values/types";
import Document "../document/types";
import CollectionIndex "../collectionIndex/types";

module Types {
    public type Type  = {#Collection};
    public type Value = {
        var autoId        : Nat                             ;  // Next id for a document on create
        typeName          : Text                            ;  // Name of the collection. Ie "Cars"
        var structure     : Trie.Trie<Text, Values.Type>     ;  // Structure of collection Record Field Name -> ValueType
        var documents     : Trie.Trie<Nat, Document.Value>  ;  // Backing Data
        var indicies      : Trie.Trie<Text, List.List<CollectionIndex.Value>>;  // Search Indices 
    };
};