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
            "...",
            "bind (implicit txn: Txn): Source.Bound[A]",
            "nonTxn: Source.Bound[A]");
    ClassTemplate.ST("+A")(S);
    Interface.K("Sink")
           ("set (v: A)(implicit txn: Txn): Unit",
            ":= (v: A)(implicit txn: Txn) = set(v)",
            "...",
            "bind (implicit txn: Txn): Sink.Bound[A]",
            "nonTxn: Sink.Bound[A]");
    ClassTemplate.KT("-A")(K);
    Interface.R("Ref")
           ("...",
            "bind (implicit txn: Txn): Ref.Bound[A]",
            "nonTxn: Ref.Bound[A]");
    ClassTemplate.RT("A")(R);

    h := 50;
    v := 25;

    S.nw = (0, 0);
    K.nw = S.sw + (h, -v);
    R.nw = K.sw + (-h, -v);

    % bound versions
    Interface.BS("Source.Bound")
           ("get: A",
            "unary_! = get",
            "...",
            "unbind: Source[A]",
            "context: Option[Txn]");
    ClassTemplate.BST("+A")(BS);
    Interface.BK("Sink.Bound")
           ("set (v: A): Unit",
            ":= (v: A) = set(v)",
            "...",
            "unbind: Sink[A]",
            "context: Option[Txn]");
    ClassTemplate.BKT("-A")(BK);
    Interface.BR("Ref.Bound")
           ("...",
            "unbind: Ref[A]",
            "context: Option[Txn]");
    ClassTemplate.BRT("A")(BR);

    g := h * 2.5;

    BS.nw = S.ne + (g, 0);
    BK.nw = BS.sw + (h, -v);
    BR.nw = BK.sw + (-h, -v);

    drawObjects(S, ST, K, KT, R, RT);
    drawObjects(BS, BST, BK, BKT, BR, BRT);

    link(inheritance)(R.nw + (h/2, 0) -- S.sw + (h/2, 0));
    link(inheritance)(R.nw + (h*3/2, 0) -- K.sw + (h/2, 0));

    link(inheritance)(BR.nw + (h/2, 0) -- BS.sw + (h/2, 0));
    link(inheritance)(BR.nw + (h*3/2, 0) -- BK.sw + (h/2, 0));

    PictureInfoCopy.iEquiv(iPict);
    iEquiv.iFont.name := "pcrr";
    iEquiv.iFont.scale := 8;

    sep := 8;

    EPicture.ES(iEquiv)("~");
    ES.c = BS.w + (-g/2, -25 + sep/2);
    EPicture.ESS(iEquiv)("~");
    ESS.c = ES.c + (0, -sep);
    EPicture.EK(iEquiv)("~");
    EK.c = BK.w + (-g/2 - h/4, -25 + sep/2);
    EPicture.EKK(iEquiv)("~");
    EKK.c = EK.c + (0, -sep);
    EPicture.ER(iEquiv)("~");
    ER.c = BR.w + (-g/2, -22 + sep/2);
    EPicture.ERR(iEquiv)("~");
    ERR.c = ER.c + (0, -sep);

    drawObjects(ES, ESS, EK, EKK, ER, ERR);
endfig;
end

