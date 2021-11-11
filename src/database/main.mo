import Database "database";

actor {
    public func greet(name : Text) : async Text {
        return "Hello, " # name # "!";
    };
};
