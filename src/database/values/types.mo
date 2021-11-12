module {
    public type Value = {
        #Nat       : Nat64                 ;
        #Int       : Int64                 ;
        #Float     : Float                 ;
        #Text      : Text                  ;
        #Blob      : Blob                  ;
        #Principal : Principal             ;
        //#Document  : Document.Value      ; // HZL TODO : Do we really want to support nested documents? Or, should this be a refence type. {#Local : Document.Value; #Remote ...}
        #Optional  : {v : ?Value; t : Type}; 
    };

    public type Type = {
        #Nat                               ;
        #Int                               ;
        #Float                             ;
        #Text                              ;
        #Blob                              ;
        #Principal                         ;
        //#Document   : Document.Type      ;
        #Optional   : Type                 ;
    };
}