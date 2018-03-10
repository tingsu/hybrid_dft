    $(OBJDIR)/MyUseDefAssoc.cmo:   $(OBJDIR)/usedef.cmo \
      $(OBJDIR)/reachingdefs.cmo     $(OBJDIR)/MyUseDefAssocByHand.cmo \
        $(OBJDIR)/MyDfSetting.cmo \
        $(OBJDIR)/MyCriticalEdge.cmo \
        $(OBJDIR)/MyCilUtility.cmo  $(OBJDIR)/errormsg.cmi \
     $(OBJDIR)/cil.cmi
    $(OBJDIR)/MyUseDefAssoc.cmx:   $(OBJDIR)/usedef.cmx \
      $(OBJDIR)/reachingdefs.cmx     $(OBJDIR)/MyUseDefAssocByHand.cmx \
        $(OBJDIR)/MyDfSetting.cmx \
        $(OBJDIR)/MyCriticalEdge.cmx \
        $(OBJDIR)/MyCilUtility.cmx  $(OBJDIR)/errormsg.cmx \
     $(OBJDIR)/cil.cmx
