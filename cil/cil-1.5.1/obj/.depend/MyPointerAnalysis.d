    $(OBJDIR)/MyPointerAnalysis.cmo: \
        $(OBJDIR)/MyUseDefAssocByHand.cmo \
        $(OBJDIR)/MyDfSetting.cmo \
        $(OBJDIR)/FindCil.cmo  $(OBJDIR)/errormsg.cmi  $(OBJDIR)/cil.cmi
    $(OBJDIR)/MyPointerAnalysis.cmx: \
        $(OBJDIR)/MyUseDefAssocByHand.cmx \
        $(OBJDIR)/MyDfSetting.cmx \
        $(OBJDIR)/FindCil.cmx  $(OBJDIR)/errormsg.cmx  $(OBJDIR)/cil.cmx
