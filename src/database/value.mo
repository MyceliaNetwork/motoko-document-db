import Hash "mo:base/Hash";
import Order "mo:base/Order";
import Trie "mo:base/Trie";
import Blob "mo:base/Blob";
import Nat64 "mo:base/Nat64";
import Int64 "mo:base/Int64";
import Int "mo:base/Int";
import Float "mo:base/Float";
import Principal "mo:base/Principal";
import Text "mo:base/Text";
import Option "mo:base/Option";
import P "mo:base/Prelude";

module Value = {
    public type Value = {
        #Nat       : Nat64                 ;
        #Int       : Int64                 ;
        #Float     : Float                 ;
        #Text      : Text                  ;
        #Blob      : Blob                  ;
        #Principal : Principal             ;
        //#Document  : Document.Value        ; // HZL TODO : Do we really want to support nested documents? Or, should this be a refence type. {#Local : Document.Value; #Remote ...}
        #Optional  : {v : ?Value; t : Type}; 
    };

    public type Type = {
        #Nat                               ;
        #Int                               ;
        #Float                             ;
        #Text                              ;
        #Blob                              ;
        #Principal                         ;
        //#Document   : Document.Type        ;
        #Optional   : Type                 ;
    };

    public func hash(v : Value.Value) : Hash.Hash {
        switch(v) {
            case (#Nat(v)) {Hash.hash(Nat64.toNat(v))};
            case (#Int(v)) {Int.hash(Int64.toInt(v))};
            case (#Float(v)) {Text.hash(Float.toText(v))};
            case (#Text(v)) {Text.hash v};
            case (#Principal(v)) {Principal.hash v};
            case (#Blob(v)) {Blob.hash v};
            //case (#Document(v)) {Document.hash(v)};
            case (#Optional(v)) {
                switch(v.v) {
                    case null return 0;
                    case (?value) return hash(value);
                };
            };
        };
    };

    public func compare(l : Value.Value, r : Value.Value) : Order.Order {
        assert sameType(typeOf(l), typeOf(r));

        switch(l) {
            case (#Nat(v)) {
                return Nat64.compare(v, Unwrap.nat(r));
            };
            case (#Int(v)) {
                return Int64.compare(v, Unwrap.int(r));
            };
            case (#Float(v)) {
                return Float.compare(v, Unwrap.float(r));
            };
            case (#Text(v)) {
                return Text.compare(v, Unwrap.text(r));
            };
            case (#Principal(v)) {
                return Principal.compare(v, Unwrap.principal(r));
            };
            case (#Blob(v)) {
                return Blob.compare(v, Unwrap.blob(r));
            };
            // case (#Document(v)) {
            //     return Nat.compare(v.id, Unwrap.document(r).id);
            // };
            case (#Optional(v)) {
                let rR = Unwrap.optional(r);
                switch(v.v) {
                    case (null) {
                        if (Option.isNull(rR)) {
                            return #equal;
                        };
                        return #less;
                    };
                    case (?v)  {
                        switch rR {
                            case null return #greater;
                            case (?rRValue) {return compare(v, rRValue)};
                        };
                    };
                };
            };
        };
    };
    
    module Unwrap {
        public func nat(v0 : Value) : Nat64 {
            switch(v0) { case (#Nat(v)) return v; case _ {assert false}};
            P.unreachable();
        };

        public func int(v0 : Value) : Int64 {
            switch(v0) { case (#Int(v)) return v; case _ {assert false}};
            P.unreachable();
        };

        public func float(v0 : Value) : Float {
            switch(v0) { case (#Float(v)) return v; case _ {assert false}};
            P.unreachable();
        };

        public func text(v0 : Value) : Text {
            switch(v0) { case (#Text(v)) return v; case _ {assert false}};
            P.unreachable();
        };

        public func principal(v0 : Value) : Principal {
            switch(v0) { case (#Principal(v)) return v; case _ {assert false}};
            P.unreachable();
        };

        public func blob(v0 : Value) : Blob {
            switch(v0) { case (#Blob(v)) return v; case _ {assert false}};
            Blob.fromArray([]);
        };

        // public func document(v0 : Value) : Document.Value {
        //     switch(v0) { case (#Document(v)) return v; case _ {assert false}};
        //     P.unreachable();
        // };

        public func optional(v0 : Value) :?Value.Value {
            switch(v0) { case (#Optional(v)) return v.v; case _ {assert false}};
            P.unreachable();
        };
    };

    public func key(v : Value) : Trie.Key<Value> {
        {key = v; hash = hash(v)};
    };

    public func equal(l : Value, r : Value) : Bool {
        return hash(l) == hash(r) and typeHash(typeOf(l)) == typeHash(typeOf(r));
    };

    public func typeOf(v : Value) : Type {
        switch(v) {
            case (#Nat(_))       {#Nat};
            case (#Int(_))       {#Int};
            case (#Text(_))      {#Text};
            case (#Principal(_)) {#Principal};
            //case (#Document(v))  {#Document(v.docType)};
            case (#Blob(_))      {#Blob};
            case (#Float(_))     {#Float};
            case (#Optional(v))  {#Optional(v.t)};
        };
    };

    public func typeHash(v : Type) : Hash.Hash {
        switch (v) {
            case (#Nat(_))       {0};
            case (#Int(_))       {1};
            case (#Text(_))      {2};
            case (#Principal(_)) {3};
            //case (#Document(v))  {4};
            case (#Blob(_))      {5};
            case (#Optional(v))  {return 1000 + typeHash(v) + 1;};
            case (#Float(_))         {100}
        };
    };

    public func sameType(l : Value.Type, r : Value.Type) : Bool {
        return typeHash(l) == typeHash(r);
    };
};