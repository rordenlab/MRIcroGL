unit UserNotification;
//MacOS transient dialogs

interface
{$ifdef Darwin}
{$mode objfpc}{$H+}
{$modeswitch objectivec1}
{$linkframework Foundation}

uses
  Classes, SysUtils, MacOSAll, CocoaAll, CocoaUtils, LCLType;//, CarbonProc;

const
  NSUserNotificationActivationTypeNone = 0;
  NSUserNotificationActivationTypeContentsClicked = 1;
  NSUserNotificationActivationTypeActionButtonClicked = 2;

type
  NSUserNotificationPtr = ^NSUserNotification;
  NSUserNotificationCenterPtr = ^NSUserNotificationCenter;
  NSUserNotificationCenterDelegateProtocolPtr = ^NSUserNotificationCenterDelegateProtocol;
  NSUserNotificationActivationType = NSInteger;
  NSUserNotificationActivationTypePtr = ^NSUserNotificationActivationType;
  NSUserNotification = objcclass external (NSObject)
  private
    _internal: id;
  public
    procedure setTitle(newValue: NSString); message 'setTitle:';
    function title: NSString; message 'title';
    procedure setSubtitle(newValue: NSString); message 'setSubtitle:';
    function subtitle: NSString; message 'subtitle';
    procedure setInformativeText(newValue: NSString); message 'setInformativeText:';
    function informativeText: NSString; message 'informativeText';
    procedure setActionButtonTitle(newValue: NSString); message 'setActionButtonTitle:';
    function actionButtonTitle: NSString; message 'actionButtonTitle';
    procedure setUserInfo(newValue: NSDictionary); message 'setUserInfo:';
    function userInfo: NSDictionary; message 'userInfo';
    procedure setDeliveryDate(newValue: NSDate); message 'setDeliveryDate:';
    function deliveryDate: NSDate; message 'deliveryDate';
    procedure setDeliveryTimeZone(newValue: NSTimeZone); message 'setDeliveryTimeZone:';
    function deliveryTimeZone: NSTimeZone; message 'deliveryTimeZone';
    procedure setDeliveryRepeatInterval(newValue: NSDateComponents); message 'setDeliveryRepeatInterval:';
    function deliveryRepeatInterval: NSDateComponents; message 'deliveryRepeatInterval';
    function actualDeliveryDate: NSDate; message 'actualDeliveryDate';
    function isPresented: boolean; message 'isPresented';
    function isRemote: boolean; message 'isRemote';
    procedure setSoundName(newValue: NSString); message 'setSoundName:';
    function soundName: NSString; message 'soundName';
    procedure setHasActionButton(newValue: boolean); message 'setHasActionButton:';
    function hasActionButton: boolean; message 'hasActionButton';
    function activationType: NSUserNotificationActivationType; message 'activationType';
    procedure setOtherButtonTitle(newValue: NSString); message 'setOtherButtonTitle:';
    function otherButtonTitle: NSString; message 'otherButtonTitle';
  end;

  NSUserNotificationCenterDelegateProtocol = objcprotocol;

  NSUserNotificationCenter = objcclass external (NSObject)
  private
    _internal: id;
  public
    class function defaultUserNotificationCenter: NSUserNotificationCenter; message 'defaultUserNotificationCenter';
    //procedure setDelegate(newValue: NSUserNotificationCenterDelegateProtocol); message 'setDelegate:';
    procedure setDelegate(newValue: NSObject); message 'setDelegate:';
    function delegate: NSUserNotificationCenterDelegateProtocol; message 'delegate';
    procedure setScheduledNotifications(newValue: NSArray); message 'setScheduledNotifications:';
    function scheduledNotifications: NSArray; message 'scheduledNotifications';
    procedure scheduleNotification (notification: NSUserNotification); message 'scheduleNotification:';
    procedure removeScheduledNotification (notification: NSUserNotification); message 'removeScheduledNotification:';
    function deliveredNotifications: NSArray; message 'deliveredNotifications';
    procedure deliverNotification (notification: NSUserNotification); message 'deliverNotification:';
    procedure removeDeliveredNotification (notification: NSUserNotification); message 'removeDeliveredNotification:';
    procedure removeAllDeliveredNotifications; message 'removeAllDeliveredNotifications';
  end;

  NSUserNotificationCenterDelegateProtocol = objcprotocol external name 'NSUserNotificationCenterDelegate' (NSObjectProtocol)
  optional
    procedure userNotificationCenter_didDeliverNotification (center: NSUserNotificationCenter; notification: NSUserNotification); message 'userNotificationCenter:didDeliverNotification:';
    procedure userNotificationCenter_didActivateNotification (center: NSUserNotificationCenter; notification: NSUserNotification); message 'userNotificationCenter:didActivateNotification:';
    function userNotificationCenter_shouldPresentNotification (center: NSUserNotificationCenter; notification: NSUserNotification): boolean; message 'userNotificationCenter:shouldPresentNotification:';
  end;

  NSUserNotificationCenterAlwaysShowDelegate = objcclass(NSObject)
  public
    function shouldPresentNotification ({%H-}center: NSUserNotificationCenter; {%H-}notification: NSUserNotification): boolean; message 'userNotificationCenter:shouldPresentNotification:';
  end;

procedure DeliverUserNotification(const Title, Subtitle, Message: string);
procedure DeliverUserNotification(const Title, Subtitle, Message, Sound: string);
procedure ShowAlertSheet(FormHandle: HWND; const TitleStr, MessageStr: string);

var
  NSUserNotificationDefaultSoundName: NSString { available in 10_8, NA }; cvar; external;

{$endif}

implementation

{$ifdef Darwin}

procedure ShowAlertSheet(FormHandle: HWND; const TitleStr, MessageStr: string);
var
  tNSStr,mNSStr, okNSStr, fNSStr: NSString;
  theWindow : CocoaAll.NSWindow;
  theID : id;
begin
  theID := NSView(FormHandle).window;
  theWindow := NSView(FormHandle).window;
  tNSStr := NSString(CFStringCreateWithPascalString(kCFAllocatorDefault, TitleStr, kCFStringEncodingUTF8));//title
  mNSStr := NSString(CFStringCreateWithPascalString(kCFAllocatorDefault, MessageStr, kCFStringEncodingUTF8));//message
  okNSStr := NSString(CFStringCreateWithPascalString(kCFAllocatorDefault, 'OK', kCFStringEncodingUTF8));//button caption
  fNSStr := NSString(CFStringCreateWithPascalString(kCFAllocatorDefault, '%@', kCFStringEncodingUTF8));//format
  NSBeginAlertSheet(tNSStr,okNSStr,nil,nil,theWindow,theID,nil,nil,nil,fNSStr, mNSStr);
end;

var
  UserNotificationCenterDelegate: NSUserNotificationCenterAlwaysShowDelegate;

function NSUserNotificationCenterAlwaysShowDelegate.shouldPresentNotification (center: NSUserNotificationCenter; notification: NSUserNotification): boolean;
begin
  Result := True;
end;

procedure DeliverUserNotification(const Title, Subtitle, Message: string);
begin
  DeliverUserNotification(Title, Subtitle, Message, CFStringToStr(CFStringRef(NSUserNotificationDefaultSoundName)));

end;

procedure DeliverUserNotification(const Title, Subtitle, Message, Sound: string);
var
  Notification: NSUserNotification;
  TmpNSStr: NSString;
begin
  Notification := NSUserNotification.alloc.init;
  try
    TmpNSStr := NSString(CFStringCreateWithPascalString(kCFAllocatorDefault, Title, kCFStringEncodingUTF8));
    try
      Notification.setTitle(TmpNSStr);
    finally
      TmpNSStr.release;
    end;
    TmpNSStr := NSString(CFStringCreateWithPascalString(kCFAllocatorDefault, Subtitle, kCFStringEncodingUTF8));
    try
      Notification.setSubtitle(TmpNSStr);
    finally
      TmpNSStr.release;
    end;
    TmpNSStr := NSString(CFStringCreateWithPascalString(kCFAllocatorDefault, Message, kCFStringEncodingUTF8));
    try
      Notification.setInformativeText(TmpNSStr);
    finally
      TmpNSStr.release;
    end;
    if Sound <> '' then
    begin
      TmpNSStr := NSString(CFStringCreateWithPascalString(kCFAllocatorDefault, Sound, kCFStringEncodingUTF8));
      try
        Notification.setSoundName(TmpNSStr);

      finally
        TmpNSStr.release;
      end;
    end;
    NSUserNotificationCenter.defaultUserNotificationCenter.deliverNotification(Notification);
  finally
    Notification.release;
  end;
end;

initialization
  UserNotificationCenterDelegate := NSUserNotificationCenterAlwaysShowDelegate.alloc.init;
  NSUserNotificationCenter.defaultUserNotificationCenter.setDelegate(UserNotificationCenterDelegate);

finalization
  NSUserNotificationCenter.defaultUserNotificationCenter.setDelegate(nil);
  UserNotificationCenterDelegate.release;
{$endif}
end.
