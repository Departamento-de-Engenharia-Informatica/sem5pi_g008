
export default interface IMedicalConditionDTO {
    domainId?: number;
    code: string;
    designation: string;
    description: string;
    symptomsList: string[];
}
