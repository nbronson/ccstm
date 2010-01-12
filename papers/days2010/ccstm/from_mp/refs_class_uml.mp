string metauml_defaultFont;
metauml_defaultFont := "ul9r8r";
string metauml_defaultFontOblique;
metauml_defaultFontOblique := "ul9ro8r";
string metauml_defaultFontBold;
metauml_defaultFontBold := "ul9b8r";
string metauml_defaultFontBoldOblique;
metauml_defaultFontBoldOblique := "ul9bo8r";

input metauml;

beginfig(1);
    % hide the locks
    iInterface.accessWidth := 0;
    iInterface.iLock.width := 0;
    iInterface.iLock.height := 0;
    iInterface.iLock.faceColor := 0.9white;
    iInterface.iLock.topColor := 0.9white;
    iInterface.iLock.rightColor := 0.9white;
    iInterface.iLock.lockColor := 0.9white;

    % base versions
    Interface.S("Source")
           ("get (implicit txn: Txn): A",
            "unary_! (implicit txn: Txn) = get",
            "bind (implicit txn: Txn): Source.Bound[A]",
            "nonTxn: Source.Bound[A]",
            "...");
    ClassTemplate.ST("+A")(S);
    Interface.K("Sink")
           ("set (v: A)(implicit txn: Txn): Unit",
            ":= (v: A)(implicit txn: Txn) = set(v)",
            "bind (implicit txn: Txn): Sink.Bound[A]  ",
            "nonTxn: Sink.Bound[A]",
            "...");
    ClassTemplate.KT("-A")(K);
    Interface.R("Ref")
           ("bind (implicit txn: Txn): Ref.Bound[A]",
            "nonTxn: Ref.Bound[A]");
    ClassTemplate.RT("A")(R);

    S.nw = (0, 0);
    K.nw = S.sw + (0, -20);

    R.w = S.se + (80, -10);

    % bound versions
    Interface.BS("Source.Bound")
           ("get: A",
            "unary_! = get",
            "unbind: Source[A]",
            "context: Option[Txn]",
            "...");
    ClassTemplate.BST("+A")(BS);
    Interface.BK("Sink.Bound")
           ("set (v: A): Unit",
            ":= (v: A) = set(v)",
            "unbind: Sink[A]",
            "context: Option[Txn]",
            "...");
    ClassTemplate.BKT("-A")(BK);
    Interface.BR("Ref.Bound")
           ("unbind: Ref[A]",
            "context: Option[Txn]");
    ClassTemplate.BRT("A")(BR);

    BS.n = K.s + (0, -40);
    BK.nw = BS.sw + (0, -20);

    BR.w = BS.se + (80 + (K.width - BK.width + R.width - BR.width) / 2, -10);

    drawObjects(S, ST, K, KT, R, RT);
    drawObjects(BS, BST, BK, BKT, BR, BRT);

    link(inheritance)(pathStepX(R.w + (0, 10), S.e, -40));
    link(inheritance)(pathStepX(R.w + (0, -10), K.e, -40));

    link(inheritance)(pathStepX(BR.w + (0, 10), BS.e, -40 - (R.width - BR.width) / 2));
    link(inheritance)(pathStepX(BR.w + (0, -10), BK.e, -40 - (R.width - BR.width) / 2));
endfig;
end

