import IMedicalRecordDTO from "../../dto/IMedicalRecordDTO";

export default interface IFreeTextService{
    
    addFreeText(medicalRecord: IMedicalRecordDTO): Promise<any>;
}