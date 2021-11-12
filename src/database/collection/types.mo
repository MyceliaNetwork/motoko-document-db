import Trie "mo:base/Trie";
import List "mo:base/List";

import Values "../values";
import Document "../document/types";
import CollectionIndex "../collectionIndex/types";

module Types {
    public type Type  = {#Collection};
    public type Value = {
        var autoId        : Nat        ;  // Next id for a document on create
        typeName          : Text       ;  // Name of the collection. Ie "Cars"
        var structure     : Structure  ;  // Structure of collection Record Field Name -> ValueType
        var documents     : Documents  ;  // Backing Data
        var indicies      : Indices    ;  // Search Indices 
    };

   public type Structure = Trie.Trie<Text, Values.Type>;
   public type Documents = Trie.Trie<Nat, Document.Value>;
   public type Indices   = Trie.Trie<Text, List.List<CollectionIndex.Value>>;
};