#include "pch.h"
#include "macAppRunner.h"
#include <cocoa/cocoa.h>
#include "platform/application.h"

using namespace platform;

// Implement platform global interface
namespace platform
{
    SharedPtr<IAppRunner> CreateAppRunner(SharedPtr<IApplication> application)
    {
        return SharedPtr<IAppRunner>(new MacAppRunner(application));
    }
}

MacAppRunner::MacAppRunner(SharedPtr<IApplication> app)
    : m_app(app)
{

}

int32 MacAppRunner::Run()
{
    m_app->Initialize();
    
    
    
    m_app->Shutdown();
    
    return 0;
}
