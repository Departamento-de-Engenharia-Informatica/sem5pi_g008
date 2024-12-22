import IMedicalConditionDTO from "../../dto/IMedicalConditionDTO";

export default interface IMedicalConditionService  {
    createMedicalCondition(medicalCondition: IMedicalConditionDTO): Promise<any>;
    searchMedicalConditionsCode(query: any): Promise<IMedicalConditionDTO[]>;
    searchMedicalConditionsDesignation(query: any): Promise<IMedicalConditionDTO[]>;

    getAllMedicalConditions(): Promise<IMedicalConditionDTO[]>;
}
