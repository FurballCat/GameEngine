#pragma once

namespace input
{
    typedef uint8 InputPlayerID;
    
    // Gameplay specific action.
    struct Action
    {
        Name m_name;
        float m_value;
        InputPlayerID m_player;
    };
    
    // An action group that can be activated or deactivated at runtime based on the gameplay context.
    // Example groups: exploration, interaction, combat, vehicle, dialog.
    // Each player can customize her/his mapping.
    // Creation methods uses prototype action mapping group, as actions are defined by gameplay design, not the user.
    // User can only choose player id and inputs mapped to given actions.
    // Each action has it's mapping description - this is because the same action can be mapped differently on keyboar and pad (axis and triggers case).
    class ActionMappingGroup
    {
    public:
        
    private:
        InputPlayerID m_player;
        // TODO: add associated user ids and device ids
    };
    
    struct InputEvent;
    
    // Bakes action groups into some optimized map of inputs to actions.
    class ActionMapping
    {
    public:
        Action Map(InputEvent& evt);
        
    private:
        using ActionMappingGroupPtr = SharedPtr<ActionMappingGroup>;
        DynArray<ActionMappingGroupPtr> m_activeGroups;
    };
}
