%string metauml_defaultFont;
%metauml_defaultFont := "ul9r8r";
%string metauml_defaultFontOblique;
%metauml_defaultFontOblique := "ul9ro8r";
%string metauml_defaultFontBold;
%metauml_defaultFontBold := "ul9b8r";
%string metauml_defaultFontBoldOblique;
%metauml_defaultFontBoldOblique := "ul9bo8r";

input metauml;

beginfig(1);
    iActivity.foreColor := (0.8, 0.8, 1.0);
    iState.foreColor := (1.0, 1.0, 0.85);

    Begin.b;

    Activity.a_ar("after-rollback", "handlers");
    Activity.a_body("body of", "atomic block");
    a_body.info.foreColor := (0.9, 1.0, 0.9);
    Activity.a_bc("before-commit", "handlers");

    State.a("Active(0)", "")(a_ar, a_body, a_bc);
    a.info.left := a.info.right := 10;
    %a.info.drawNameLine := 1;
    topToBottom(20)(a_ar, a_body, a_bc);


    Activity.aa_ar("after-rollback", "handlers");
    Activity.aa_body("body of", "atomic block");
    aa_body.info.foreColor := (0.9, 1.0, 0.9);

    State.aa("Active(1)", "")(aa_ar, aa_body);
    aa.info.left := aa.info.right := 10;
    %aa.info.drawNameLine := 1;
    topToBottom(20)(aa_ar, aa_body);


    Activity.aaa_ar("after-rollback", "handlers");
    Activity.aaa_body("body of", "atomic block");
    aaa_body.info.foreColor := (0.9, 1.0, 0.9);

    State.aaa("Active(2) ...", "")(aaa_ar, aaa_body);
    aaa.info.left := aaa.info.right := 10;
    %aaa.info.drawNameLine := 1;
    topToBottom(20)(aaa_ar, aaa_body);


    Activity.preparing_rr("read-resource", "validate");
    Activity.preparing_wr("write-resource", "prepare");

    State.preparing("Preparing", "")(preparing_rr, preparing_wr);
    preparing.info.left := preparing.info.right := 10;
    %preparing.info.drawNameLine := 1;
    topToBottom(20)(preparing_rr, preparing_wr);


    Activity.prepared_d("external", "decider");

    State.prepared("Prepared", "")(prepared_d);
    prepared.info.left := prepared.info.right := 10;
    %prepared.info.drawNameLine := 1;


    State.undecided("")(a, aa, aaa, preparing, prepared);    
    undecided.info.left := undecided.info.right := 20;
    %undecided.info.drawNameLine := 1;
    undecided.info.foreColor := (1.0, 1.0, 0.95);

    aa.nw = a.ne + (30, 0);
    aaa.nw = aa.ne + (30, 0);
    %preparing.width = a.width;
    %prepared.width = a.width;
    topToBottom(40)(aa, preparing);
    prepared_d.c = preparing_wr.c + (aaa_body.c - aa_body.c);

    Activity.committed_wr("write-resource", "commit");
    Activity.committed_ac("after-commit", "handlers");
    End.committed_e;

    State.committed("Committed", "")(committed_wr, committed_ac, committed_e);
    committed.info.left := committed.info.right := 20;
    %committed.info.drawNameLine := 1;
    topToBottom(20)(committed_wr, committed_ac, committed_e);


    Activity.rolledback_wr("write-resource", "rollback");
    Activity.rolledback_ar("after-rollback", "handlers");
    End.rolledback_e;

    State.rolledback("RolledBack", "")(rolledback_wr, rolledback_ar, rolledback_e);
    rolledback.info.left := rolledback.info.right := 20;
    %rolledback.info.drawNameLine := 1;
    topToBottom(20)(rolledback_wr, rolledback_ar, rolledback_e);

    %committed.width = a.width;
    leftToRight(70)(b, a_body);
    topToBottom(50)(prepared, committed);
    rolledback.c = committed.c - (aaa_body.c - a_body.c);

    State.attempt("Txn lifetime - one atomic execution attempt", "")(b, undecided, committed, rolledback);
    attempt.info.foreColor := (1.0, 1.0, 0.95);
    %drawObjects(b, undecided, committed, rolledback);
    drawObject(attempt);

    clink(transition)(b, a_body);
    clink(transition)(a_ar, a_body);
    clink(transition)(a_body, a_bc);
    pair a_i;
    pair a_ii;
    a_i = (a_body.c + aa_body.c) / 2 + (0, -5);
    a_ii = (a_body.c + aa_body.c) / 2 + (0, 5);
    link(transition)(pathCut(a_body, aa_body)(a_body.c .. a_ii .. aa_body.c));
    link(transition)(pathCut(aa_body, a_body)(aa_body.c .. a_i .. a_body.c));
    item(iAssoc)("nested atomic")(obj.s = a_ii);
    item(iAssoc)("nested commit")(obj.n = a_i);
    link(transition)(pathCut(aa_body, a_ar)(aa_body.c -- a_ar.c));
    item(iAssoc)("partial rollback")(obj.n = 0.5[a_ar.c,aa_ar.c] + (0, -10));
    pair aa_i;
    pair aa_ii;
    aa_i = (aa_body.c + aaa_body.c) / 2 + (0, -5);
    aa_ii = (aa_body.c + aaa_body.c) / 2 + (0, 5);
    link(transition)(pathCut(aa_body, aaa_body)(aa_body.c .. aa_ii .. aaa_body.c));
    link(transition)(pathCut(aaa_body, aa_body)(aaa_body.c .. aa_i .. aa_body.c));
    clink(transition)(aa_ar, aa_body);
    clink(transition)(aaa_body, aa_ar);
    clink(transition)(aaa_ar)(aaa_body);
    clink(transition)(a_bc, preparing_rr);
    clink(transition)(preparing_rr, preparing_wr);
    clink(transition)(preparing_wr, prepared_d);
    clink(transition)(preparing_wr, committed_wr);
    clink(transition)(prepared_d, committed_wr);
    clink(transition)(committed_wr, committed_ac);
    clink(transition)(committed_ac, committed_e);
    clink(transition)(undecided, rolledback_wr);
    clink(transition)(rolledback_wr, rolledback_ar);
    clink(transition)(rolledback_ar, rolledback_e);

endfig;
end












































































































