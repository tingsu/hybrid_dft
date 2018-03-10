    $(OBJDIR)/MyDataflowTesting.cmo: \
        $(OBJDIR)/MyUseDefAssocByHand.cmo \
        $(OBJDIR)/MyUseDefAssoc.cmo \
        $(OBJDIR)/MyPointerAnalysis.cmo \
        $(OBJDIR)/MyIntermediateGoals.cmo \
        $(OBJDIR)/MyIfConditionMap.cmo \
        $(OBJDIR)/MyDfSetting.cmo  $(OBJDIR)/errormsg.cmi \
     $(OBJDIR)/cil.cmi
    $(OBJDIR)/MyDataflowTesting.cmx: \
        $(OBJDIR)/MyUseDefAssocByHand.cmx \
        $(OBJDIR)/MyUseDefAssoc.cmx \
        $(OBJDIR)/MyPointerAnalysis.cmx \
        $(OBJDIR)/MyIntermediateGoals.cmx \
        $(OBJDIR)/MyIfConditionMap.cmx \
        $(OBJDIR)/MyDfSetting.cmx  $(OBJDIR)/errormsg.cmx \
     $(OBJDIR)/cil.cmx
