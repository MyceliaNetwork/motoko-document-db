import Buffer "mo:base/Buffer";
import Collection "../collection";
import CollectionIndex "../collectionIndex";
import Document "../document/types";
import List "mo:base/List";
import Nat64 "mo:base/Nat64";
import Option "mo:base/Option";
import Result "mo:base/Result";
import Values "../values";

module {

    // Goals MLP
    // 1. Let users get a document by id
    // 2. Let users get documents matching a single field
    public module V1 {
        public type FieldValue = {fieldName : Text; op : Operators;};

        public type LiteralValue = {
            #Field : FieldValue;
        };

        public type FilterType = {
                #Not     : FilterType;
                #And     : [FilterType];
                #Or      : [FilterType];

                #Field : FieldValue;
        };

        public type Operators = {
            #Equal : Values.Value;
        };

        public type Types = {
            #Filter : FilterType; // Finds all matching record
        };

        public type Error = {
            #TypeMismatch : Text;
            #NoIndex      : Text;
        };

        public type HandlerResult = Result.Result<?Collection.PublicValue, Error>;


        public func executeQuery(queryRequest : Types, c : Collection.Value) {
            switch(queryRequest) {
                case (#Filter(v)) {

                };
            };
        };

        func handleFilterQuery(f : FilterType, c : Collection.Value) : Result.Result<[Collection.PublicValue], Error> {
            let out = Buffer.Buffer<Collection.PublicValue>(0);

            var res : HandlerResult = #ok(null);
            switch(f) {
                case (#Not(v)) {

                };
                case (#And(v)) {

                };
                case (#Or(v)) {

                };
                case (#Field(v)) {
                    if (v.fieldName == "id") {
                        res := handleId(v, c);
                    };
                };
            };
            return #ok(out.toArray());
        };

        func handleId(f : FieldValue, c : Collection.Value) : HandlerResult {
            switch(f.op) {
                case (#Equal(wrappedValue)) {
                    switch(wrappedValue) {
                        case (#Nat(id)) {
                            return #ok(Collection.getById(c, Nat64.toNat(id)));
                        };
                        case (_) {
                            return #err(#TypeMismatch("Query on id field requires type Nat but got " # debug_show wrappedValue))
                        };
                    };
                };
            };
        };

        func isValidIndexForEqual(v : CollectionIndex.IndexType) : Bool {
            switch(v) {
                case (#Hash(_))    return true;
                case (#Ordered(_)) return true;
            };
        };

        func isApplicableIndex(f : FieldValue, idx : CollectionIndex.IndexType) : Bool {
                switch(f.op) {
                    case (#Equal(_)) {
                        return isValidIndexForEqual(idx);
                    };
                };
                return false;
        };

        func findInIndex(f : FieldValue, c : Collection.Value, idx : CollectionIndex.Value) : [Collection.PublicValue] {
            switch(idx.value) {
                case (#SingleField(v)) {
                    
                };
                case (#Unique(v)) {

                };
            };
            
            return [];
        };

        func handleField(f : FieldValue, c : Collection.Value) : HandlerResult {
            let indicies : ?List.List<CollectionIndex.Value> = Collection.getIndicesForField(c, f.fieldName);

            if (Option.isNull(indicies)) {
                return #err(#NoIndex(""))
            };

            let bestIndex : ?CollectionIndex.Value = do ? {
                List.find<CollectionIndex.Value>(indicies!, func(v) {
                    switch(v.value) {
                        case (#SingleField(idx)) {
                            return isApplicableIndex(f, idx);
                        };
                        case (#Unique(idx)) {
                            return isApplicableIndex(f, idx);
                        };
                    };
                    return false;
                })!;
            };

            if (Option.isNull(bestIndex)) {
                return #err(#NoIndex(""));
            };

            let a = do ? {

            };

            return #err(#NoIndex(""));
        };
    };
}