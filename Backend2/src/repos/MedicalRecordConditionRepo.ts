import {Inject, Service} from "typedi";
import mongoose, {Document, Model} from "mongoose";
import {IMedicalRecordConditionPersistence} from "../dataschema/IMedicalRecordConditionPersistence";
import {MedicalRecordCondition} from "../domain/MedicalRecordCondition/MedicalRecordCondition";
import IMedicalRecordConditionRepo from "../services/IRepos/IMedicalRecordConditionRepo";
import {MedicalRecordConditionMapper} from "../mappers/MedicalRecordConditionMapper";


@Service()
export default class MedicalRecordConditionRepo implements IMedicalRecordConditionRepo{
  constructor(@Inject('medicalRecordConditionSchema') private medicalRecordConditionSchema: Model<IMedicalRecordConditionPersistence & Document>,) {
  }
  public async getMedicalRecordConditionById(s: string): Promise<MedicalRecordCondition> {
    console.log('RepoESTOYRepoESTOYRepoESTOYRepoESTOY');
    const medicalRecordCondition = await this.medicalRecordConditionSchema.findOne({id:s});

    return MedicalRecordConditionMapper.toDomain(medicalRecordCondition);
  }
  exists(t: MedicalRecordCondition): Promise<boolean> {
    //TODO:Implement exists method
    return Promise.resolve(false);
  }

  public async save(medicalRecordCondition: MedicalRecordCondition): Promise<MedicalRecordCondition> {

    const id = await this.getLastId();

    const rawMedicalRecordCondition: any = MedicalRecordConditionMapper.toPersistence(medicalRecordCondition, id);

    const medicalRecordConditionCreated = await this.medicalRecordConditionSchema.create(rawMedicalRecordCondition);

    return MedicalRecordConditionMapper.toDomain(medicalRecordConditionCreated);
  }


  public async getLastId(): Promise<number> {

    let number = 1;

    const medicalRecord = await this.medicalRecordConditionSchema.find();

    if (medicalRecord.length > 0) {
      number = medicalRecord.length + 1;
    }

    return number;
  }

  public async getMedicalRecordConditionsWithIds(medicalRecordId: string): Promise<any[]> {
   
      const objectId = new mongoose.Types.ObjectId(medicalRecordId);

      const medicalRecordConditions = await this.medicalRecordConditionSchema.find({ medicalRecordId: objectId });
      
      medicalRecordConditions.map(MedicalRecordConditionMapper.toDomain);
      
      return medicalRecordConditions;
  }

  public async getMedicalRecordConditionByMedicalRecordIdAndConditionId(medicalRecordId: string, medicalConditionCode: string): Promise<any> {
    
    const objectId = new mongoose.Types.ObjectId(medicalRecordId);
    
    const medicalRecordCondition = await this.medicalRecordConditionSchema.findOne({ medicalRecordId: objectId, conditionId : medicalConditionCode });
    
    if(!medicalRecordCondition) {
      return undefined;
    }
    
    return MedicalRecordConditionMapper.toDomain(medicalRecordCondition);
  }

  public async getAllMedicalRecordConditions(): Promise<MedicalRecordCondition> {
    const medicalRecordConditions = await this.medicalRecordConditionSchema.find();
    return MedicalRecordConditionMapper.toDomain(medicalRecordConditions);

  }

}
