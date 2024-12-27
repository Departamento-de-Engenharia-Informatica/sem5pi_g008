import {Repo} from "../../core/infra/Repo";
import {MedicalRecord} from "../../domain/MedicalRecord/MedicalRecord";

export default interface IMedicalRecordRepo extends Repo<MedicalRecord>{
    save(medicalRecord: MedicalRecord, medicalRecordId?: string): Promise<MedicalRecord>;
    getAll(): Promise<MedicalRecord[]>;
    getMedicalRecordByDomainId(medicalRecordId: string): Promise<any>;
    getMedicalRecordById(medicalRecordId: string): Promise<MedicalRecord>;
    saveFamilyHistory(medicalRecord: Promise<any>, familylist: any[]): any;
}
