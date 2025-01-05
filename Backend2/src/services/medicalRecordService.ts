import {Inject, Service} from "typedi";
import IMedicalRecordService from "./IServices/IMedicalRecordService";
import config from "../../config";
import IMedicalRecordRepo from "./IRepos/IMedicalRecordRepo";
import {MedicalRecord} from "../domain/MedicalRecord/MedicalRecord";
import IMedicalRecordAllergyRepo from "./IRepos/IMedicalRecordAllergyRepo";
import IMedicalRecordAllergyDTO from "../dto/IMedicalRecordAllergyDTO";
import {MedicalRecordAllergyMapper} from "../mappers/MedicalRecordAllergyMapper";
import IAllergyRepo from "./IRepos/IAllergyRepo";
import IMedicalRecordConditionRepo from "./IRepos/IMedicalRecordConditionRepo";
import IMedicalRecordConditionDTO from "../dto/IMedicalRecordConditionDTO";
import {NoMedicalRecordConditionsException} from "../domain/MedicalRecordCondition/NoMedicalRecordConditionsException";
import {MedicalRecordConditionMapper} from "../mappers/MedicalRecordConditionMapper";
import IStaffDetailsDTO from "../dto/IStaffDetailsDTO";
import http from "node:http";
import IMedicalConditionRepo from "./IRepos/IMedicalConditionRepo";
import {NoMedicalRecordException} from "../domain/MedicalRecord/NoMedicalRecordException";
import {
    MedicalConditionNotFoundException
} from "../domain/MedicalCondition/Exceptions/MedicalConditionNotFoundException";
import {
    MedicalRecordConditionNotFoundException
} from "../domain/MedicalRecordCondition/MedicalRecordConditionNotFoundException";
import {Designation} from "../domain/Shared/designation";

import IMedicalRecordFreeTextDTO from "../dto/IMedicalRecordFreeTextDTO";
import IMedicalRecordFamilyHistoryRepo from "./IRepos/IMedicalRecordFamilyHistoryRepo";
import {MedicalRecordFreeTextMap} from "../mappers/MedicalRecordFreeTextMapper";
import IMedicalRecordFreeTextRepo from "./IRepos/IMedicalRecordFreeTextRepo";
import {Code} from "../domain/Shared/code";



@Service()
export default class MedicalRecordService implements IMedicalRecordService{
    constructor(
        @Inject(config.repos.medicalRecord.name) private medicalRecordRepo:IMedicalRecordRepo,
        @Inject(config.repos.medicalRecordAllergy.name) private medicalRecordAllergyRepo: IMedicalRecordAllergyRepo,
        @Inject(config.repos.allergy.name) private allergyRepo: IAllergyRepo,
        @Inject(config.repos.medicalRecordFreeText.name) private medicalRecordFreeTextRepo: IMedicalRecordFreeTextRepo,
        @Inject(config.repos.medicalRecordCondition.name) private medicalRecordConditionRepo: IMedicalRecordConditionRepo,
        @Inject(config.repos.medicalCondition.name) private medicalConditionRepo: IMedicalConditionRepo,
        @Inject(config.repos.medicalRecordFamilyHistory.name) private medicalRecordFamilyHistory: IMedicalRecordFamilyHistoryRepo
    ) {}


    public async createMedicalRecord(medicalRecordId:string): Promise<void> {
        const medicalProps = {};
        const medicalRecord = MedicalRecord.create(medicalProps);
        await this.medicalRecordRepo.save(medicalRecord.getValue(), medicalRecordId);
    }
    async createFamilyHistory(medicalRecordID: any, familyHistory: any) {
        const medicalRecord = this.medicalRecordRepo.getMedicalRecordByDomainId(medicalRecordID);

        if (!medicalRecord) {
            throw new Error(`Medical record with ID ${medicalRecordID} not found`);
        }
       console.log('Dto in Service', familyHistory);
        console.log('guardar');
        await this.medicalRecordFamilyHistory.save(familyHistory);
    }
    async getMedicalRecordFamilyHistoryWithIds(medicalRecordId: string): Promise<any[]> {
        const medicalRecordFamilyHistory = await this.medicalRecordFamilyHistory.getMedicalRecordFamilyHistoryWithIds(medicalRecordId);
        return medicalRecordFamilyHistory;
    }
    

    public async updateMedicalRecordConditionComment(id: string, newComment:string): Promise<IMedicalRecordConditionDTO> {
        console.log('id', id);
        const medicalRecordCondition = await this.medicalRecordConditionRepo.getByDomainId(Number.parseInt(id));
        console.log('medicalRecordCondition', medicalRecordCondition);
        console.log('pass1');
        medicalRecordCondition.comment = newComment;
        console.log('pass2');
        await this.medicalRecordConditionRepo.updateUsingDomainId(medicalRecordCondition, "comment");
        console.log('pass3');
        return MedicalRecordConditionMapper.toDTO(medicalRecordCondition);
    }

    public async updateMedicalRecordAllergiesComment(id: string, newComment:string): Promise<IMedicalRecordAllergyDTO> {
        const medicalRecordAllergy = await this.medicalRecordAllergyRepo.getByDomainId(Number.parseInt(id));
        medicalRecordAllergy.comment = newComment;
        await this.medicalRecordAllergyRepo.updateUsingDomainId(medicalRecordAllergy, "comment");
        return MedicalRecordAllergyMapper.toDTO(medicalRecordAllergy);
    }

    public async getAllergies(medicalRecordId:string): Promise<IMedicalRecordAllergyDTO[]> {


        const medicalRecord = await this.medicalRecordRepo.getMedicalRecordByDomainId(medicalRecordId);
        const medicalRecordPrivateId = medicalRecord.props._id.toString();

        const medicalRecordAllergies = await this.medicalRecordAllergyRepo.getByMedicalId(medicalRecordPrivateId);

        console.log(medicalRecordAllergies);

        let aux : IMedicalRecordAllergyDTO[] = [];

        for(let i = 0; i < medicalRecordAllergies.length; i++){
            const dto = await MedicalRecordAllergyMapper.toDTO(medicalRecordAllergies[i]);
            aux.push(await this.fixDto(dto));
        }

        return aux;
    }

    private async fixDto(dto: IMedicalRecordAllergyDTO): Promise<IMedicalRecordAllergyDTO> {
        const allergy = await this.allergyRepo.getById(dto.allergy);
        dto.allergy = allergy.allergy;
        dto.doctor = await this.getDoctorName(dto.doctor);
        return dto;
    }

    private async getDoctorName(doctorId: string): Promise<string> {
        return "Doctor";
    }

    async getAllMedicalRecordConditions(): Promise<IMedicalRecordConditionDTO[]> {
        const medicalRecordConditionList = await this.medicalRecordConditionRepo.getAllMedicalRecordConditions();

        if (medicalRecordConditionList.length === 0) {
            throw new NoMedicalRecordConditionsException();
        }
        console.log('medicalRecordConditionList2');
        const medicalRecordConditionDTOList = [];
  

        for (const medicalRecordCondition of medicalRecordConditionList) {
            const medicalConditionDTO = MedicalRecordConditionMapper.toDTO(medicalRecordCondition);
            medicalRecordConditionDTOList.push(medicalConditionDTO);
        } 

        return medicalRecordConditionDTOList;

    }
 
    public async getMedicalRecordConditions(medicalRecordId: string): Promise<IMedicalRecordConditionDTO[]> {

        const medicalRecord = await this.medicalRecordRepo.getMedicalRecordByDomainId(medicalRecordId);

        if(!medicalRecord) {
            throw new NoMedicalRecordException();
        }
        console.log('medicalRecord', medicalRecord);
        console.log('medicalRecord.props._id.toString()', medicalRecord.props._id.toString());

        const medicalRecordConditionList = await this.medicalRecordConditionRepo.getMedicalRecordConditionsWithIds(medicalRecord.props._id.toString());

        if (medicalRecordConditionList.length === 0) {
            throw new NoMedicalRecordConditionsException();
        }

        const medicalRecordConditionDTOList = [];

        for (const medicalRecordCondition of medicalRecordConditionList) {

            const staffDetailsDTO = await this.getStaffDetails(medicalRecordCondition.doctorId);

            const condition = await this.medicalConditionRepo.getMedicalConditionByBusinessId(medicalRecordCondition.conditionId);

            const medicalConditionDTO = MedicalRecordConditionMapper.toDTO(medicalRecordCondition, condition, medicalRecord.id, staffDetailsDTO);

            medicalRecordConditionDTOList.push(medicalConditionDTO);
        }

        return medicalRecordConditionDTOList;
    }

    public async getMedicalRecordConditionByCode(medicalRecordId: string, conditionCode: string): Promise<IMedicalRecordConditionDTO> {

        const medicalRecord = await this.medicalRecordRepo.getMedicalRecordByDomainId(medicalRecordId);

        if(!medicalRecord) {
            throw new NoMedicalRecordException();
        }

        const code = Code.create(conditionCode);

        const medicalCondition = await this.medicalConditionRepo.getMedicalConditionByCode(code.getValue());

        if(!medicalCondition) {
            throw new MedicalConditionNotFoundException("No Medical Condition registered in the system with this Code.");
        }

        const medicalRecordCondition = await this.medicalRecordConditionRepo.getMedicalRecordConditionByMedicalRecordIdAndConditionId(
            medicalRecord.props._id.toString(), medicalCondition.props._id.toString());

        if(!medicalRecordCondition) {
            throw new MedicalRecordConditionNotFoundException("No Medical Condition found for this Code.");
        }

        const staffDetailsDTO = await this.getStaffDetails(medicalRecordCondition.doctorId);

        return MedicalRecordConditionMapper.toDTO(medicalRecordCondition, medicalCondition, medicalRecord.id, staffDetailsDTO);
    }

    public async getMedicalRecordConditionByDesignation(medicalRecordId: string, conditionDesignation: string): Promise<IMedicalRecordConditionDTO> {

        const medicalRecord = await this.medicalRecordRepo.getMedicalRecordByDomainId(medicalRecordId);

        if(!medicalRecord) {
            throw new NoMedicalRecordException();
        }

        const designation = Designation.create(conditionDesignation);

        const medicalCondition = await this.medicalConditionRepo.getMedicalConditionByDesignation(designation.getValue());

        if(!medicalCondition) {
            throw new MedicalConditionNotFoundException("No Medical Condition registered in the system with this Designation.");
        }

        const medicalRecordCondition = await this.medicalRecordConditionRepo.getMedicalRecordConditionByMedicalRecordIdAndConditionId(
            medicalRecord.props._id.toString(), medicalCondition.props._id.toString());

        if(!medicalRecordCondition) {
            throw new MedicalRecordConditionNotFoundException("No Medical Condition found for this Designation.");
        }

        const staffDetailsDTO = await this.getStaffDetails(medicalRecordCondition.doctorId);

        return MedicalRecordConditionMapper.toDTO(medicalRecordCondition, medicalCondition, medicalRecord.id, staffDetailsDTO);
    }

    private async getStaffDetails(staffId: string): Promise<IStaffDetailsDTO> {

        const url = config.Backend1.URL + '/Staff';
        const urlApi = `${url}/by-id/${staffId}`;

        return new Promise((resolve, reject) => {
            http.get(urlApi, (res) => {
                let data = '';

                res.on('data', (chunk) => {
                    data += chunk;
                });

                res.on('end', () => {

                    try {
                        const staff: IStaffDetailsDTO = JSON.parse(data);
                        resolve(staff);
                    } catch (error) {
                        resolve(undefined);
                    }
                });
            }).on('error', (error) => {
                reject(error);
            });
        });
    }

    public async addFreeText(medicalRecord: IMedicalRecordFreeTextDTO): Promise<any> {

        const privateId = await this.medicalRecordRepo.getMedicalRecordByDomainId(medicalRecord.medicalRecordId);
        medicalRecord.medicalRecordId=privateId.props._id;
        
        const medicalRecordDomain=MedicalRecordFreeTextMap.toDomain(medicalRecord);
        await this.medicalRecordFreeTextRepo.save(medicalRecordDomain);

    }

    public async getFreeTexts(medicalRecordId:string): Promise<IMedicalRecordFreeTextDTO[]> {

        const medicalRecord = await this.medicalRecordRepo.getMedicalRecordByDomainId(medicalRecordId);

        if(!medicalRecord) {
            throw new NoMedicalRecordException();
        }

        const medicalRecordPrivateId = medicalRecord.props._id.toString();
        console.log(medicalRecordPrivateId);

        const medicalRecordFreeTexts = await this.medicalRecordFreeTextRepo.getByMedicalId(medicalRecordPrivateId);
        let aux : IMedicalRecordFreeTextDTO[] = [];
        
        for(let i = 0; i < medicalRecordFreeTexts.length; i++){
            const dto = await MedicalRecordFreeTextMap.toDTO(medicalRecordFreeTexts[i]);
            aux.push(await this.fixDtoFreeText(dto));
        }

        return aux;
    }
    private async fixDtoFreeText(dto: IMedicalRecordFreeTextDTO): Promise<IMedicalRecordFreeTextDTO> {
        const doctor = await this.getStaffDetails(dto.doctorId);
        if(!doctor){
            dto.doctorId="unknown";
            return dto;
        }
        dto.doctorId = doctor.firstName + " " + doctor.lastName;
        return dto;
    }

}

