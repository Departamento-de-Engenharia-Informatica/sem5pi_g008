export interface IMedicalConditionPersistence {
    domainId: number;
    code: string;
    designation: string;
    description: string;
    symptomsList: string[];
}