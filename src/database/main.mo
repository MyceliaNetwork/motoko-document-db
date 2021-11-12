import Database "database";
import Debug "mo:base/Debug";
import Hash "mo:base/Hash";
import Iter "mo:base/Iter";
import List "mo:base/List";
import Nat "mo:base/Nat";
import Nat64 "mo:base/Nat64";
import Text "mo:base/Text";
import Trie "mo:base/Trie";

actor {
    func keyOf(v : Text) : Trie.Key<Text> {
        return {key = v; hash = Text.hash(v)};
    };

    let test : Database.Collection.Value = do {
        let v : Database.Collection.Value = {
            typeName      = "Test";
            var autoId    = 0;
            var documents : Database.Collection.Documents = Trie.empty();
            var structure : Database.Collection.Structure = Trie.empty();
            var indicies  : Database.Collection.Indices   = Trie.empty();
        };

        v.structure := Trie.put<Text, Database.Value.Type>(v.structure, keyOf("amount"), Text.equal, #Nat).0;
        v.structure := Trie.put<Text, Database.Value.Type>(v.structure, keyOf("name"),   Text.equal, #Text).0;

        var i : List.List<Database.CollectionIndex.Value> = List.nil();

        i := List.push<Database.CollectionIndex.Value>({
            name = "Test index";
            target = "amount";
            var value = #SingleField(#Hash(Trie.empty()));
        }, i);

        i := List.push<Database.CollectionIndex.Value>({
            name = "Foo index";
            target = "name";
            var value = #SingleField(#Hash(Trie.empty()));
        }, i);

        v.indicies  := Trie.put(v.indicies, keyOf("amount"), Text.equal, i).0;
        v;
    };

    public type P = Database.Document.PublicValue;

    public query func readAll() : async Trie.Trie<Nat, P> {
        var out = Trie.empty<Nat, P>();

        for (x in Trie.iter(test.documents)) {
            out := Trie.put<Nat, P>(out, {key = x.0; hash = Hash.hash(x.0)}, Nat.equal, {
                data =    x.1.data;
                docType = x.1.docType.typeName;
                id      = x.1.id;
            }).0;
        };

        return out;
    };

    public func put(times : Nat) : async () {
        for (x in Iter.range(0, times)) {
            switch(Database.Collection.create(test, [
                ("amount", #Nat(Nat64.fromNat(x))),
                ("name", #Text("Lol its record " # Nat.toText(x))),
            ])) {
                case (#ok(_)) {};
                case (#err(e)) {Debug.print(debug_show e)};
            };
        };
    };

    public query func read(id : Nat) : async ?Database.Document.PublicValue {
        Database.Collection.getById(test, id);
    };
};
