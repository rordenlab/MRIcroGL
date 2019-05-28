{$mode objfpc}
{$modeswitch objectivec2}

unit CocoaMore;
interface
uses
	Menus, StdCtrls, CocoaAll;

procedure MakePullDownButton (parent: TButton; popupMenu: TPopupMenu; bordered: boolean; offsetX, offsetY, offsetW, offsetH: integer); 

implementation

type
	TCocoaPullDownButton = objcclass (NSPopupButton)
		buttonTitle: NSString;
		popupMenu: TPopupMenu;
		procedure setTitle (aString: NSString); override;
		procedure mouseDown(event: NSEvent); override;
		procedure dealloc; override;
	end;

procedure TCocoaPullDownButton.setTitle(aString: NSString);
begin
	inherited setTitle(aString);
	buttonTitle.release;
	buttonTitle := title.copy;
end;

procedure TCocoaPullDownButton.dealloc;
begin
	buttonTitle.release;
	inherited;
end;

procedure TCocoaPullDownButton.mouseDown(event: NSEvent);
begin
	//popupMenu.Popup(-1, -1);
	setMenu(NSMenu(popupMenu.handle).copy);
	insertItemWithTitle_atIndex(buttonTitle, 0);

	inherited;
end;

procedure MakePullDownButton (parent: TButton; popupMenu: TPopupMenu; bordered: boolean; offsetX, offsetY, offsetW, offsetH: integer);
var
 	handle: NSButton; 
 	button: TCocoaPullDownButton;
 	newFrame: NSRect;
begin
	handle := NSButton(parent.Handle);

	newFrame := handle.frame;
	newFrame.origin.x += offsetX;
	newFrame.origin.y += offsetY;
	newFrame.size.width += offsetW;
	newFrame.size.height += offsetH;

	button := TCocoaPullDownButton.alloc.initWithFrame(newFrame);
	button.popupMenu := popupMenu;
	button.setMenu(NSMenu(popupMenu.handle));
	button.setPullsDown(true);
	button.setTitle(handle.title);
	button.setBordered(bordered);

	// add buttont to parent superview
	// then hide the parent
	handle.superview.addSubview(button);
	handle.setHidden(true);
end;

end.
