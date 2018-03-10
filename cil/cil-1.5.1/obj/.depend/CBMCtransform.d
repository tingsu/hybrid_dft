    $(OBJDIR)/CBMCtransform.cmo: \
        $(OBJDIR)/MyUseDefAssocByHand.cmo \
        $(OBJDIR)/MyUseDefAssoc.cmo \
        $(OBJDIR)/MyPointerAnalysis.cmo \
        $(OBJDIR)/MyDfSetting.cmo \
        $(OBJDIR)/MyCilUtility.cmo \
        $(OBJDIR)/FindCil.cmo  $(OBJDIR)/errormsg.cmi  $(OBJDIR)/cil.cmi
    $(OBJDIR)/CBMCtransform.cmx: \
        $(OBJDIR)/MyUseDefAssocByHand.cmx \
        $(OBJDIR)/MyUseDefAssoc.cmx \
        $(OBJDIR)/MyPointerAnalysis.cmx \
        $(OBJDIR)/MyDfSetting.cmx \
        $(OBJDIR)/MyCilUtility.cmx \
        $(OBJDIR)/FindCil.cmx  $(OBJDIR)/errormsg.cmx  $(OBJDIR)/cil.cmx
